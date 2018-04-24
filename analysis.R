library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

ticket_prices <- fread("C:/Users/tkonc/Documents/Data/AirlineTickets/ticket_prices.csv")
jet_fuel <- fread("C:/Users/tkonc/Documents/Data/AirlineTickets/jet_fuel.csv")
huf_currency_pairs <- fread("C:/Users/tkonc/Documents/Data/AirlineTickets/huf_currency_pairs.csv")

str(ticket_prices)
str(jet_fuel)
str(huf_currency_pairs)

ticket_prices <- ticket_prices %>%
                   mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

jet_fuel <- jet_fuel %>%
              mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
              arrange(Date)

jet_fuel <- ticket_prices %>%
              select(Date) %>%
              unique() %>%
              full_join(jet_fuel, by = "Date") %>%
              arrange(Date) %>%
              tidyr::fill(`Jet Fuel`, .direction = "down")

huf_currency_pairs <- huf_currency_pairs %>%
                        mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
                        select(Date, RUB, USD)

huf_currency_pairs <- ticket_prices %>%
                        select(Date) %>% 
                        unique() %>%
                        full_join(huf_currency_pairs, by = "Date") %>%
                        arrange(Date) %>%
                        tidyr::fill(RUB, .direction = "down") %>%
                        tidyr::fill(USD, .direction = "down")

ticket_prices <- ticket_prices %>%
                   left_join(jet_fuel, by = "Date") %>%
                   left_join(huf_currency_pairs, by = "Date")

rm(jet_fuel)
rm(huf_currency_pairs)


kerozin_vs_ticketprices <- ticket_prices %>% 
  ggplot(aes(x = `Jet Fuel` * USD))  +
  geom_point(aes(y = `Wizz Air`), color = "red", size = 2) + 
  geom_point(aes(y = Aeroflot), color = "blue", size = 2) + 
  geom_point(aes(y = `Austrian Airlines`), color = "orange", size = 2) + 
  geom_point(aes(y = `British Airways`), color = "darkgreen", size = 2)  +
  facet_grid(~`Flight Date`) +
  theme_economist() +
  labs(x = "Kerozin világpiaci ár", y= "Repjegyárak") +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 14))

ggsave(file = "kerozin_vs_ticketprices.png", plot = kerozin_vs_ticketprices, 
       dpi = 600, width = 30, height = 20, units = "cm", limitsize = FALSE)

date_vs_kerozin <- ticket_prices %>% 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = `Jet Fuel`), size = 2, color = "purple")+
  theme_economist() +
  labs(x = "Dátum", y= "Kerozin világpiaci ár") +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 14))

ggsave(file = "date_vs_kerozin.png", plot = date_vs_kerozin, 
       dpi = 600, width = 20, height = 20, units = "cm", limitsize = FALSE)

rub_vs_ticketprices <-  ticket_prices %>% 
  ggplot(aes(x = RUB)) +
  geom_point(aes(y = `Wizz Air`), color = "red", size = 2) + 
  geom_point(aes(y = Aeroflot), color = "blue", size = 2) + 
  geom_point(aes(y = `Austrian Airlines`), color = "orange", size = 2) + 
  geom_point(aes(y = `British Airways`), color = "darkgreen", size = 2)  +
  facet_grid(~`Flight Date`)+
  theme_economist() +
  labs(x = "Rubel árfolyam", y= "Repjegyárak") +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 14))

ggsave(file = "rub_vs_ticketprices.png", plot = rub_vs_ticketprices, 
       dpi = 600, width = 30, height = 20, units = "cm", limitsize = FALSE)

date_vs_ticketprices <- ticket_prices %>% 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = `Wizz Air`), color = "red", size = 2) + 
  geom_point(aes(y = Aeroflot), color = "blue", size = 2) + 
  geom_point(aes(y = `Austrian Airlines`), color = "orange", size = 2) + 
  geom_point(aes(y = `British Airways`), color = "darkgreen", size = 2)  +
  facet_grid(~`Flight Date`) +
  theme_economist() +
  labs(x = "Dátum", y= "Repjegyárak") +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 14))

ggsave(file = "date_vs_ticketprices.png", plot = date_vs_ticketprices, 
       dpi = 600, width = 30, height = 20, units = "cm", limitsize = FALSE)


ticket_prices %>%
  select(everything(), -`Flight Date`, -Date, -`Wizz Air`, -`British Airways`) %>% 
  cor()


