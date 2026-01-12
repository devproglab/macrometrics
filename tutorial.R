## ----echo=TRUE----------------------------------------------------------------
# Библиотеки необходимо установить один раз, а затем загружать командой library()
# install.packages(c("tidyusmacro", "readxl", "seasonal", "devtools", "vars", "tsibble", "tidyverse"))
# install.packages(c("latex2exp", "ggh4x", "bsvars", "bsvarSIGNs", "svars"))
# install_github("angusmoore/seasthedata", ref= "stable")
set.seed(1234)
library(tidyusmacro)
library(readxl)
library(seasonal)
library(devtools)
library(vars)
library(seasthedata)
library(tsibble)
library(tidyverse)
library(latex2exp)
library(ggh4x)
library(bsvars)
library(bsvarSIGNs)
library(svars)
library(scales)
library(showtext)
showtext_auto()
select <- dplyr::select
filter <- dplyr::filter

## ----echo=TRUE----------------------------------------------------------------
theme_set(theme_minimal() +
            theme(text = element_text(size=15),
                  legend.text = element_text(size=17),
                  strip.placement = "outside",
                  strip.text.x = element_text(size=17, angle=0),
                  strip.text.y.left = element_text(size=17, angle=0),
                  strip.text.y.right = element_text(size=17, angle=0),
                  legend.position = 'bottom',
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.ticks.y = element_line(),
                  axis.ticks.x = element_line(),
                  axis.line.x = element_line(),
                  axis.line.y = element_line()
            )
)
scale_y_continuous <- function(...) {do.call(ggplot2::scale_y_continuous, c(list(...), labels = label_number(big.mark = ' ', accuracy = 0.1)))}
tex_namer <- function(x) {
  if ('var' %in% names(x)) {
    x <- x %>% mutate(var = factor(var, levels = c("piexp", "x", "pi", "i"),
                                   labels = c(TeX("$\\pi^E_t$"), TeX("$x_t$"), TeX("$\\pi_t$"), TeX("$i_t$")), ordered = T))
  }
  if ('shock' %in% names(x)) {
    x <- x %>%
      mutate(shock = factor(shock, levels = c("piexp", "x", "pi", "i", "resid"), 
                            labels = c(TeX("$\\eta_{\\pi^E,t}$"), TeX("$\\eta_{y,t}$"), TeX("$\\eta_{\\pi,t}$"), TeX("$\\eta_{r,t}$"),
                                       TeX("Deterministic")), ordered = T))
  }
  x
}

## ----usmacro, echo=FALSE------------------------------------------------------
# Загрузка данных из FRED
usdata <- getFRED(y = "GDPC1", ye = "GDPPOT", cpi = "CPIAUCNS", i = "FEDFUNDS", keep_all = F)
# Визуализация
usdata %>%
  pivot_longer(-date) %>%
  ggplot() +
  geom_line(aes(x=date, y=value)) +
  facet_wrap(~name, scales = 'free_y') +
  labs(x = NULL, y = NULL, dictionary = c(cpi = "Inflation", i = "Ja"))

## ----echo=TRUE----------------------------------------------------------------
shadowrate <- read_xls('shadowrate_US.xls', col_names = FALSE)
shadowrate <- shadowrate %>%
  rename_with(~c('period','shadow_i')) %>%
  mutate(date = seq.Date(as.Date('1990-01-01'), by = 'month', length.out = nrow(shadowrate))) %>%
  select(-period) %>%
  group_by(date = as.Date(yearquarter(date))) %>%
  summarize(shadow_i = mean(shadow_i))
usdata <- usdata %>%
  left_join(shadowrate, by = 'date') %>%
  mutate(i = ifelse(!is.na(shadow_i), shadow_i, i)) %>%
  select(-shadow_i)

## ----us3eqdata----------------------------------------------------------------
# Преобразование данных
us3eq <- usdata %>%
  mutate(ly = log(y),
         lye = log(ye),
         lcpi = log(cpi),
         x = (ly - lye)*100,
         pi = 400*difference(lcpi)
  ) %>%
  select(date, x, pi, i) %>%
  filter(!is.na(pi))
# Визуализация
us3eq %>%
  pivot_longer(-date) %>%
  ggplot() +
  geom_line(aes(x=date, y=value)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~name, scales = 'free_y', ncol=1) +
  labs(x = NULL, y = NULL)

## ----uscpiseas----------------------------------------------------------------
# Визуализация
us3eq %>%
  select(date, pi) %>%
  mutate(q = quarter(date), y = year(date)) %>%
  ggplot() +
  geom_line(aes(x=y, y=pi)) +
  stat_summary(aes(y=pi, yintercept = after_stat(y), x=2000),
               fun = mean, geom = "hline", color = 'red', linewidth = 1, linetype = "dashed"
  ) +
  facet_wrap(~q, nrow = 1) +
  labs(x = NULL, y = NULL)

## ----usseasadj----------------------------------------------------------------
# Сезонная корректировка
pi_seas <- us3eq %>%
  select(date, pi) %>%
  seasthedata()
# Возвращение скорректированного ряда в датафрейм
us3eq <- us3eq %>%
  mutate(pi = pi_seas$pi)
# Визуализация
us3eq %>%
  select(date, pi) %>%
  mutate(q = quarter(date), y = year(date)) %>%
  ggplot() +
  geom_line(aes(x=y, y=pi)) +
  stat_summary(aes(y=pi, yintercept = after_stat(y), x=2000),
               fun = mean, geom = "hline", color = 'red', linewidth = 1, linetype = "dashed"
  ) +
  facet_wrap(~q, nrow = 1) +
  labs(x = NULL, y = NULL)

## ----usvar, echo=TRUE---------------------------------------------------------
# Оценка модели
usvar <- VAR(us3eq %>% select(x, pi, i), p=4)
# Матрицы коэффициентов
Phi <- Bcoef(usvar)
Phi

## ----usvareps-----------------------------------------------------------------
# Датафрейм с остатками модели
usvareps <- data.frame(residuals(usvar), date = us3eq$date[-c(1:4)])
# Визуализация
usvareps %>%
  pivot_longer(-date) %>%
  ggplot() +
  geom_line(aes(x=date, y=value)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~name, scales = 'free_y', ncol = 1) +
  labs(x = NULL, y = NULL)

## ----echo=TRUE----------------------------------------------------------------
summary(usvar)$covres

## ----echo=TRUE----------------------------------------------------------------
usvareps %>%
  filter(date == as.Date("2022-01-01")) 

## ----usvarid------------------------------------------------------------------
# Вспомогательная функция для обработки и рисования
source("https://raw.githubusercontent.com/devproglab/macrometrics/refs/heads/main/tidy_irf.R")
# Оценка импульсных откликов с идентификацией
usvar_irf <- vars::irf(usvar, n.ahead=20) %>%
  tidy_irf()
# Визуализация
usvar_irf %>%
  tex_namer() %>% 
  ggplot(aes(x=h)) +
  geom_line(aes(y=value), linewidth = 1) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid2(var ~ shock, scales = "free_y", labeller=label_parsed, axes = "margins", switch = "y", independent="y") +
  labs(x = NULL, y = NULL)

## ----echo = TRUE--------------------------------------------------------------
# Добавим в датафрейм инфляционные ожидания профессиональных прогнозистов
piexp <- read_xlsx("Median_PGDP_Growth.xlsx") %>%
  mutate(date = seq.Date(from = as.Date("1968-10-01"), 
                         by = "quarter", length.out = n())) %>%
  select(date, piexp = DPGDP2)
us3eq_exp <- piexp %>%
  left_join(us3eq) %>%
  filter(!is.na(x))
# Переоценим модель и отклики
usvar_exp <- VAR(us3eq_exp %>% select(piexp, x, pi, i), p=4)
usvar_irf_exp <- vars::irf(usvar_exp, n.ahead = 20) %>%
  tidy_irf()

## ----usvar_exp----------------------------------------------------------------
# Визуализация
usvar_irf_exp %>%
  tex_namer() %>%
  ggplot(aes(x=h)) +
  geom_line(aes(y=value), linewidth = 1) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid2(var ~ shock, scales = "free_y", labeller=label_parsed, axes = "margins", switch = "y", independent="y") +
  labs(x = NULL, y = NULL)

## ----usvar_exp_eta----------------------------------------------------------------
B <- usvar_exp %>% id.chol()
shocks <- solve(B$B) %*% t(residuals(usvar_exp))
usvar_exp_eta <- data.frame(t(shocks), us3eq_exp$date[-c(1:4)]) %>%
  rename_with(~c('piexp', 'x', 'pi', 'i', 'date'))
# Визуализация
recessions <- read_xlsx('recessions.xlsx') %>%
  filter(Peak >= usvar_exp_eta$date[1])
usvar_exp_eta %>%
  pivot_longer(-date, names_to = 'shock') %>%
  tex_namer() %>%
  ggplot() +
  geom_line(aes(x=date, y=value)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_rect(data = recessions, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf),  alpha = 0.5, inherit.aes = FALSE) +
  facet_wrap(~shock, scales = 'free_y', ncol=1, labeller = label_parsed) +
  labs(x = NULL, y = NULL)

## ----ussvar_exp_hd------------------------------------------------------------
usvar_exp_hd <- usvar_exp %>%
  id.chol() %>%
  tidy_hd(series = 2)
usvar_exp_hd %>%
  left_join(
    data.frame(t = seq(1, max(usvar_exp_hd$t)),
               date = us3eq_exp$date[-c(1:4)])
  ) %>%
  tex_namer() %>%
  ggplot() +
  geom_bar(aes(x=date, y=value, fill=shock), position="stack", stat="identity") +
  stat_summary(aes(x=date, y=value, group = 1), fun = sum, geom = "line", color = 'black', linewidth = 0.75) +
  scale_fill_discrete(name = NULL, labels = scales::parse_format()) +
  labs(x = NULL, y = NULL)

## ----ussvar_exp_fevd----------------------------------------------------------
usvar_exp_fevd <- usvar_exp %>%
  fevd() %>%
  tidy_fevd()
usvar_exp_fevd %>%
  tex_namer() %>%
  ggplot() +
  geom_bar(aes(x=h, y=value, fill=shock), position="stack", stat="identity") +
  facet_wrap(~var, labeller=label_parsed) +
  scale_fill_discrete(name = NULL, labels = scales::parse_format()) +
  labs(x = NULL, y = NULL)

## ----echo = TRUE, results = 'hide', cache = TRUE------------------------------
usbvar_exp <- us3eq_exp %>% 
  select(piexp, x, pi, i) %>%
  as.matrix() %>%
  specify_bsvar$new(p=4, stationary = rep(TRUE,4)) %>%
  estimate(S = 1000, show_progress = FALSE) %>%
  estimate(S = 10000, show_progress = FALSE)
usbvar_irf_exp <- usbvar_exp %>% 
  compute_impulse_responses(horizon = 20) %>%
  tidy_irf()

## ----usbvar_exp---------------------------------------------------------------
# Визуализация
usbvar_irf_exp %>%
  tex_namer() %>%
  ggplot(aes(x=h)) +
  geom_line(aes(y=value), linewidth = 1) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid2(var ~ shock, scales = "free_y", labeller=label_parsed, axes = "margins", switch = "y", independent="y") +
  labs(x = NULL, y = NULL)

## ----echo=TRUE----------------------------------------------------------------
# Создадим массив знаковых ограничений
sign_restrictions <- array(NA, dim = c(3, 3, 1), 
                           dimnames = list(variable = c("output_gap", "inflation", "interest_rate"), 
                                           shock = c("demand", "supply", "monetary_policy"), horizon = 1))
# Monetary policy shock:
sign_restrictions["interest_rate", "monetary_policy", 1] <- 1
sign_restrictions["output_gap", "monetary_policy", 1] <- -1
# sign_restrictions["inflation", "monetary_policy", 1] <- -1
# Demand shock:
sign_restrictions["interest_rate", "demand", 1] <- 1
sign_restrictions["inflation", "demand", 1] <- 1
sign_restrictions["output_gap", "demand", 1] <- 1
# Supply shock:
sign_restrictions["interest_rate", "supply", 1] <- 1
sign_restrictions["inflation", "supply", 1] <- 1
sign_restrictions["output_gap", "supply", 1] <- -1
sign_restrictions

## ----echo=TRUE, results='hide', cache = TRUE----------------------------------
# Оценим модель со знаковыми ограничениями
usbvar_sign <- us3eq %>%
  select(x, pi, i) %>%
  as.matrix() %>%
  specify_bsvarSIGN$new(p = 4, stationary = rep(TRUE, 3), sign_irf = sign_restrictions) %>%
  estimate(S = 1000)
usbvar_irf_sign <- usbvar_sign %>%
  compute_impulse_responses(horizon = 20) %>%
  tidy_irf(varlist = c('x', 'pi', 'i'))

## ----usbsvarsign--------------------------------------------------------------
# Визуализация
usbvar_irf_sign %>%
  tex_namer() %>%
  ggplot(aes(x=h)) +
  geom_line(aes(y=value), linewidth = 1) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid2(var ~ shock, scales = "free_y", labeller=label_parsed, axes = "margins", switch = "y", independent="y") +
  labs(x = NULL, y = NULL)

## ----echo=TRUE----------------------------------------------------------------
# Создадим массив знаковых ограничений
signzero_restrictions <- array(NA, dim = c(3, 3, 4), 
                               dimnames = list(variable = c("interest_rate", "output_gap", "inflation"), 
                                               shock = c("monetary_policy", "demand", "supply"), horizon = 1:4))
# Monetary policy shock:
signzero_restrictions["interest_rate", "monetary_policy", 1] <- 1
signzero_restrictions["inflation", "monetary_policy", 1] <- 0
signzero_restrictions["inflation", "monetary_policy", 2:4] <- -1
signzero_restrictions["output_gap", "monetary_policy", 1:4] <- -1
# Demand shock:
signzero_restrictions["interest_rate", "demand", 1] <- 1
signzero_restrictions["inflation", "demand", 1] <- 1
signzero_restrictions["output_gap", "demand", 1] <- 1
# Supply shock:
signzero_restrictions["interest_rate", "supply", 1] <- 1
signzero_restrictions["inflation", "supply", 1] <- 1
signzero_restrictions["output_gap", "supply", 1] <- -1

## ----echo=TRUE, results='hide', cache = TRUE----------------------------------
# Оценим модель со знаковыми и нулевыми ограничениями
usbvar_signzero <- us3eq %>%
  select(i, x, pi) %>%
  as.matrix() %>%
  specify_bsvarSIGN$new(p = 4, stationary = rep(TRUE, 3), sign_irf = signzero_restrictions) %>%
  estimate(S = 100)
usbvar_irf_signzero <- usbvar_signzero %>%
  compute_impulse_responses(horizon = 20) %>%
  tidy_irf(varlist = c('i', 'x', 'pi'))

## ----usbsvarsignzero----------------------------------------------------------
# Визуализация
usbvar_irf_signzero %>%
  tex_namer() %>%
  ggplot(aes(x=h)) +
  geom_line(aes(y=value), linewidth = 1) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid2(var ~ shock, scales = "free_y", labeller=label_parsed, axes = "margins", switch = "y", independent="y") +
  labs(x = NULL, y = NULL)

## ----echo=TRUE----------------------------------------------------------------
sign_narrative <- list(
  specify_narrative(start = which(us3eq$date == as.Date('1979-10-01')), periods = 1, type = "S", sign = 1, shock = 3),  
  specify_narrative(start = which(us3eq$date == as.Date('1979-10-01')), periods = 1, type = "B", sign = 1, shock = 3, var = 3)
  #,specify_narrative(start = which(us3eq$date == as.Date('1974-04-01')), periods = 1, type = "S", sign = 1, shock = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('1974-04-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('1988-10-01')), periods = 1, type = "S", sign = 1, shock = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('1988-10-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('1994-01-01')), periods = 1, type = "S", sign = 1, shock = 3),  
  # specify_narrative(start = which(us3eq$date == as.Date('1994-01-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('1990-10-01')), periods = 1, type = "S", sign = -1, shock = 3), 
  # specify_narrative(start = which(us3eq$date == as.Date('1990-10-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('1998-10-01')), periods = 1, type = "S", sign = -1, shock = 3),  
  # specify_narrative(start = which(us3eq$date == as.Date('1998-10-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('2001-04-01')), periods = 1, type = "S", sign = -1, shock = 3),  
  # specify_narrative(start = which(us3eq$date == as.Date('2001-04-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('2003-01-01')), periods = 1, type = "S", sign = -1, shock = 3),
  # specify_narrative(start = which(us3eq$date == as.Date('2003-01-01')), periods = 1, type = "A", sign = 1, shock = 3, var = 3)
)

## ----echo=TRUE, results='hide', cache = TRUE----------------------------------
# Оценим модель со знаковыми нарративными ограничениями
usbvar_sign_narr <- us3eq %>%
  select(x, pi, i) %>%
  as.matrix() %>%
  specify_bsvarSIGN$new(p = 4, 
                        stationary = rep(TRUE, 3),
                        sign_irf = sign_restrictions, 
                        sign_narrative = sign_narrative) %>%
  estimate(S = 1000)
usbvar_irf_sign_narr <- usbvar_sign_narr %>%
  compute_impulse_responses(horizon = 20) %>%
  tidy_irf(varlist = c('x', 'pi', 'i'))

## ----usbsvarsignnarr----------------------------------------------------------
# Визуализация
usbvar_irf_sign_narr %>%
  tex_namer() %>%
  ggplot(aes(x=h)) +
  geom_line(aes(y=value), linewidth = 1) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid2(var ~ shock, scales = "free_y", labeller=label_parsed, axes = "margins", switch = "y", independent="y") +
  labs(x = NULL, y = NULL)

