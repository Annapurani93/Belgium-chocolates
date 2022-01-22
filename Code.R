library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
tuesdata$chocolate->choc
unique(choc$specific_bean_origin_or_bar_name)
choc%>%
  filter(company_location=="Belgium")%>%
  distinct()%>%
  select(-c(ref,company_location,ingredients, country_of_bean_origin,specific_bean_origin_or_bar_name))%>%
  group_by(company_manufacturer,review_date)%>%
  arrange(company_manufacturer,desc(review_date))%>%
  slice(which.max(review_date))%>%
  data.frame()%>%
  group_by(company_manufacturer)%>%
  arrange(company_manufacturer,desc(rating))%>%
  slice(which.max(rating))%>%
  select(-review_date)->data

data%>%
  separate(cocoa_percent,into=c("num","percent"),sep="%")%>%
  select(-percent)->Data

as.numeric(Data$num)->Data$num

Data%>%
  mutate(percent=(num/100))%>%
  mutate(other=1-percent)%>%
  unite(x, c(company_manufacturer, most_memorable_characteristics), sep = " - Texture: ", remove = TRUE)%>%
  select(x,percent,other)%>%
  gather("Segment","Share",2:3)->data1

hsize <- 3

data1 <- data1 %>% 
  mutate(h = hsize)

ggplot(data1, aes(x = hsize, y = Share, fill = Segment)) +
  geom_col(colour="white", width = 1) +
  scale_fill_manual(values = c("black","brown")) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(~x,ncol=3)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="THE HIGHEST-RATED CHOCOLATES IN BELGIUM CONTAIN MORE THAN 50% COCOA",
       subtitle = str_wrap("Chocolate plays an important part in the Belgian economy. There are over 2,000 chocolatiers in the country, and 172,000 tonnes of chocolate are produced each year and widely exported. But what is the share of cocoa in the the top-rated Belgium chocolates, and what do the chocolates actually taste like? Find out from the visualization below",150),
       caption="Data via Tidy Tuesday| Design and Analysis: @annapurani93")->img


ggsave("Belgium.png",img,width=14,height=25.5)  

