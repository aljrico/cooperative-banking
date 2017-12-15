
# Studying data of Cooperative Banking -----------------------------
# Authors: Alejandro Jiménez Rico <aljrico@gmail.com> & Albert Xavier Lòpez


# Libraries ---------------------------------------------------------------

library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


# Load Data ---------------------------------------------------------------

raw.data <- fread("data/data.csv")

#Data without missing values
raw.data[ raw.data == "na" ] <- NA
df <- na.omit(raw.data)



# Exploratory Analysis ----------------------------------------------------

ggplot(df) +
	geom_bar(aes(x=Country, fill = Country)) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
	scale_fill_viridis(discrete=TRUE, begin=0.1, end=0.9)

ggplot(df) +
	geom_bar(aes(x=as.factor(Año), fill = as.factor(Año))) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
	scale_fill_viridis(discrete=TRUE, begin=0.1, end=0.9)

df %>%
	group_by(Country) %>%
	summarise(N = sum(as.numeric(Clients))) %>%
	ggplot() +
	geom_bar(aes(x=as.factor(Country), y = N, fill = as.factor(Country)), stat = "identity") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
	scale_fill_viridis(discrete=TRUE, begin=0.1, end=0.9) +
	labs(y="Number of Clients", x = "Country")

df %>%
	group_by(Country) %>%
	summarise(N = sum(as.numeric(Clients))/sum(as.numeric(Staff))) %>%
	ggplot() +
	geom_bar(aes(x=as.factor(Country), y = N, fill = as.factor(Country)), stat = "identity") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
	scale_fill_viridis(discrete=TRUE, begin=0.1, end=0.9) +
	labs(y="Clients/Staff", x = "Country")

