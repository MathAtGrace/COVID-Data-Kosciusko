library(lubridate)
load("S_all.Rdata")

mCS = 45
S_all %>%
  rowwise() %>%
  do(data.frame(.[1:6], date = seq(.$started, .$end_date, by = "1 day"))) %>%
  filter(test_result == "p") %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year == 2021 & location != "Before") %>%
  ggplot(aes(x = date, fill = location)) + 
  geom_dotplot(method = "histodot",
               binwidth = 1,
               stackgroups = TRUE,
               dotsize = 1,
               stackratio = adjust) +
  scale_x_date(date_labels="%b %d",
               date_breaks  = "7 days",
               minor_breaks = "1 day",
               limits = c(spring, last)) +
  scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c_vals) +
  # Make this as high as the tallest column
  coord_fixed(ratio = max(mCS,5)) +
  ggtitle("Active Confirmed Cases Among Students Each Day") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.position="bottom") + 
  ylab("Students") + 
  xlab("Date")+
  geom_hline(yintercept =  seq(0, mCS+1, 10)/mCS, linetype = "dotted")+
  geom_vline(xintercept =  seq(spring, last, "months"))


#Include the Before's
S_all %>%
  rowwise() %>%
  do(data.frame(.[1:6], date = seq(.$started, .$end_date, by = "1 day"))) %>%
  filter(test_result == "p") %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year == 2021) %>%
  ggplot(aes(x = date, fill = location)) + 
  geom_dotplot(method = "histodot",
               binwidth = 1,
               stackgroups = TRUE,
               dotsize = 1,
               stackratio = adjust) +
  scale_x_date(date_labels="%b %d",
               date_breaks  = "7 days",
               minor_breaks = "1 day",
               limits = c(spring, last)) +
  scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c_vals) +
  # Make this as high as the tallest column
  coord_fixed(ratio = max(mCS,5)) +
  ggtitle("Active Confirmed Cases Among Students Each Day") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.position="bottom") + 
  ylab("Students") + 
  xlab("Date")+
  geom_hline(yintercept =  seq(0, mCS+1, 10)/mCS, linetype = "dotted")+
  geom_vline(xintercept =  seq(spring, last, "months"))
