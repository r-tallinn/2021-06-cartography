#' ---
#' title: "Analyze CRAN download data for packages released since 1.1.2020"
#' author: "Andreas Beger"
#' date: "2021-06-29"
#' ---

library(dplyr)
library(ggplot2)
library(here)

# Assuming this is staring out in 2021-06-cartography

#
#   Clean data ----
#   _________

dl <- readRDS(here("r-trending/data/downloads.rds"))

# Packages were first released at different dates to CRAN; use days since
# first release to normalize this stuff
dl <- dl %>%
  group_by(package) %>%
  mutate(
    days_since_release = as.integer(date - min(date)),
    days_up = max(days_since_release)) %>%
  ungroup()

# Take out 0 counts in the days before a packages was released
dl <- dl %>%
  group_by(package) %>%
  filter(date >= min(date)) %>%
  ungroup()

# Add some package-level stats
dl <- dl %>%
  group_by(package) %>%
  mutate(
    avg_daily_count = mean(count),
    med_daily_count = median(count),
    total_count = sum(count)
  ) %>%
  ungroup()

# Create a version aggregated to "package" as unit/rows
by_pack <- dl %>%
  group_by(package) %>%
  summarize(
    avg_daily_count = mean(count),
    med_daily_count = median(count),
    total_count = sum(count),
    days_up = unique(days_up),
    rstudio = max(rstudio)
  )


#
#   First look ----
#   ___________

# Plot: What does this look like?
dl %>%
  ggplot(aes(x = days_since_release, y = count, group = package)) +
  geom_line(alpha = 0.5) +
  theme_minimal()

# Uuuf there are some crazy outliers
dl %>%
  filter(count > 50000) %>%
  select(date, package, count, avg_daily_count, med_daily_count)

# What is 'terra'?
# > Methods for spatial data analysis with raster and vector data.
# > Author:	Robert J. Hijmans ORCID iD [cre, aut], Roger Bivand ORCID iD [ctb],
# > Karl Forner [ctb], Jeroen Ooms ORCID iD [ctb], Edzer Pebesma ORCID iD [ctb]
#
# Ok, this is legit, keep it in.
#


# Some packages have had 0 counts, pull these out
any0 <- dl %>%
  filter(count==0) %>%
  count(package) %>%
  arrange(desc(n))

# Check whether any of these have had non-trivial max daily downloads
dl %>%
  filter(package %in% any0$package) %>%
  select(package, avg_daily_count, med_daily_count) %>%
  group_by(package) %>%
  slice(1) %>%
  arrange(desc(avg_daily_count))

# Some of these are legit, so keep them in. Even if they make plotting with
# log axis more difficult.


# Make a plot for the slides
dl %>%
  filter(avg_daily_count > 30) %>%
  filter(days_up > 120) %>%
  ggplot(aes(x = days_since_release, y = count, group = package)) +
  geom_line(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Days since first release", y = "Daily downloads")
ggsave(here("docs/news/img/cran-downloads.png"), height = 5, width = 8)


#
#   Second attempt: linear model residuals ----
#   _____________________________________
#
#   Aside from the outlier daily spikes for some packages, another pattern that
#   seems relatively obvious in the plot above is that some packages jump
#   right away to a crazy high level of downloads. Those are utility packages
#   that are used as dependencies in a lot of other packages, not packages
#   that are per se downloaded by a lot of users.
#
#   Easier to see if we focus on top median download packages:
#

top_med_downloads <- by_pack %>%
  arrange(desc(med_daily_count)) %>%
  head(10)

dl %>%
  filter(package %in% top_med_downloads$package) %>%
  ggplot(aes(x = days_since_release, y = count, group = package)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~ package)

#   Those level shifts correspond to when a package was added as a reverse
#   dependency by another, popular package. For example, **conquer** was picked
#   up as a dependency by **quantreg**; **cachem** is used in **shiny** and
#   **memoise**, among other packages.
#
#   The top average and median downloads are dominated like packages like that.
#
#   I suppose in principle one could check when a package was added by another
#   package as reverse dependency and try adjust in some fashion for indirect
#   downloads due to that other package. Too much work though.
#
#   What about looking at residuals for a stupid simple model based on how
#   long a package has been up and whether it is by RStudio?

ggplot(by_pack, aes(x = days_up, y = total_count, color = factor(rstudio))) +
  geom_point(alpha = 0.5) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 1e4, 1e5, 1e6, 1e7),
                labels = c("1", "10", "100", "1k", "10k", "100k", "1 mil", "10 mil")) +
  scale_x_continuous(trans = "log1p", breaks = c(0, 10, 100, 500)) +
  theme_minimal()

#   Let's try it out

mdl <- lm(log(total_count) ~ log1p(days_up) + rstudio, data = by_pack)

by_pack$resid <- residuals(mdl)

by_pack %>%
  arrange(desc(resid)) %>%
  select(package, days_up, total_count, rstudio, resid) %>%
  head(10)

# Ok, I get some packages that are _not_ RStudio. Good enough.
#
# On the plot below, try to add some isolines for residuals
isolines <- expand.grid(
  resid = c(2, 4),
  days_up = c(0, 500),
  rstudio = 0
)
isolines$total_count <- exp(predict(mdl, newdata = isolines) + isolines$resid)

ggplot(by_pack, aes(x = days_up, y = total_count)) +
  geom_line(data = isolines, aes(group = resid), color = "gray80",
            linetype = "dashed") +
  annotate("text", x = c(1, 1), y = c(250, 2000), label = c("+2", "+4"),
           color = "gray80") +
  geom_point(size = 1.5, color = "black") +
  geom_point(size = 1, aes(color = resid)) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 1e4, 1e5, 1e6, 1e7),
                labels = c("1", "10", "100", "1k", "10k", "100k", "1 mil", "10 mil")) +
  scale_x_continuous(trans = "log1p", breaks = c(0, 10, 100, 500)) +
  theme_minimal() +
  scale_color_gradient2()


by_pack %>%
  arrange(desc(resid)) %>%
  filter(resid > 2) %>%
  knitr::kable()

# https://cran.r-project.org/package=progressr



