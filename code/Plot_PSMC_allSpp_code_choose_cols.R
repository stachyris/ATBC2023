library(stringr)
library(ggplot2)
library(deeptime)

#Saving the path to folders Nilgiris and Palani
parent.folder="Z:/Nilgiris_Demography/Analysis/R plots/scaled"

setwd(parent.folder)

#Saving the list of file names along with path
all_files <- list.files(path = parent.folder, pattern = ".*_combined_(\\d+).*.0.txt$", full.names = TRUE, recursive = TRUE)

length(all_files) #396

#Reading all the files into a list/array
data_list=lapply(all_files, read.table, header=FALSE)

#Just checking
View(data_list[[1]])

#Selecting only the first two rows from each file and saving files name and info in extra cols
for (i in 1:length(data_list)){data_list[[i]]<-cbind(data_list[[i]][,1:2],
                                                     str_split_1(all_files[i],'/')[6],
                                                     str_split_1(all_files[i],'/')[7],
                                                     str_split_1(all_files[i],'/')[8])}
#Combining all files into a single file
combined <- do.call("rbind", data_list) 

View(combined)

colnames(combined) <- c("Ya", "Ne", "Mountain", "Species", "FileName")

## Adding more columns
combined$Sample <- sub("(.*?_.*?)_.*", "\\1", combined[,5])
combined$Run <- str_split_i(combined[,5], "\\.", i=4)

str(combined)

## Checks
unique(combined$Species)
#"ALT"     "BLT"     "BOF"     "BTG"     "NC"      "NP"      "NS"      "PP" 
#"YBB"     "BOF_PAL" "BTG_PAL" "NP_PAL"  "PLT"     "PP_PAL"  "WBS"     "YBB_PAL"

## Changing species names to be uniform

combined[combined$Species=="NC",4] <- "NLT"
combined[combined$Species=="NS",4] <- "RBS"
combined$Species <- gsub("_PAL","",combined$Species)

unique(combined$Species)
#"ALT" "BLT" "BOF" "BTG" "NLT" "NP"  "NS"  "PP"  "YBB" "PLT" "WBS"

table(combined$Species)

combined$Species <- factor(combined$Species, levels = c("NLT", "PLT", "BLT", "ALT", "RBS", "WBS", "BOF", "YBB", "NP", "BTG", "PP"))
summary(combined$Species)

## Habitat
combined$Habitat <- "Forest"
combined[grep("NP|PP|BTG", combined$Species),8] <- "Grassland"
combined[grep("YBB", combined$Species), 8] <- "Forest Generalist"
combined[grep("NLT|PLT|BLT|ALT|RBS|WBS|BOF", combined$Species),8] <- "Forest Specialists"
combined[grep("NP", combined$Species), 8] <- "Grassland Specialist"
combined[grep("BTG|PP", combined$Species), 8] <- "Grassland Generalist"

table(combined$Habitat)


## Add site variable to spp name

table(combined$Mountain) 
#Ashambu        Banasura        Nilgiris Palani_Anamalai
#   2079            2079            9251           10637

combined$Site.Species = paste0(combined$Mountain, ".", combined$Species)
unique(combined$Site.Species)

combined$Site.Species <- gsub("Nilgiris","N",combined$Site.Species)
combined$Site.Species <- gsub("Palani_Anamalai","P",combined$Site.Species)
combined$Site.Species <- gsub("Banasura","B",combined$Site.Species)
combined$Site.Species <- gsub("Ashambu","A",combined$Site.Species)

View(table(combined$Site.Species))

combined$Site.Species = factor(combined$Site.Species, 
                               levels=c("N.NLT","P.PLT","B.BLT","A.ALT",
                                        "N.RBS","P.WBS","N.BOF","P.BOF",
                                        "N.YBB","P.YBB","N.NP","P.NP",
                                        "N.BTG","P.BTG","N.PP","P.PP"))

levels(combined$Site.Species)

#####################################################
#####################################################


length(combined[combined$Ya==0,1]) #396 lines where Ya = 0, so 1 occurrence per file
combined <- combined[combined$Ya!=0,]

summary(combined)



## Library pals to get enough colours
library("pals")
colours <- pal.bands(n=36,polychrome)
help(stepped)

as.vector(polychrome(16))


colours = c("#3283FE","#325A9B","#3E9FB3","#0F8299","#FEAF16","#FBE426",
            "#1C8356","#1CBE4F","#5A5156","#E4E1E3",
            "#DEA0FD","#D85FF7","#C4451C","#85660D","#F8A19F","#F6222E")

p = ggplot(combined, aes(x=Ya/1000, y=Ne, group = Site.Species, col= Site.Species), ) +
  geom_path() + scale_x_log10(limits=c(0,2200)) + 
  ggtitle("Demographic history: Habitat generalists and specialists") +
  scale_colour_manual(values=colours) +
  annotate("rect", xmin = c(14,130,337,424), xmax = c(29,191,374,478), ymin = -Inf, ymax = Inf,
           alpha = 0.5, fill = "#CDCDCD") + 
  annotate("text", x=c(21,160,355,465), y=c(100,100,100,100), 
           label=c("MIS2", "MIS6", "MIS10", "MIS12"), cex=2.5)

p + xlab("Years ago * 10^3") + ylab("Effective Population Size (* 10^4)") + 
  scale_x_log10(limits=c(10,2200), breaks=c(10,20,50,100,200,500,1000,2000)) +
  theme_classic() 


#################################
####################################

# Filter the dataframe for Forest Generalist and Grassland Generalist species
filtered_data <- combined[combined$Habitat %in% c("Forest Generalist", "Grassland Generalist"), ]

# Scatterplot generation
p <- ggplot(filtered_data, aes(x = Ya / 1000, y = Ne, group = Site.Species, col = Site.Species)) +
  geom_path() +
  scale_x_log10(limits = c(10, 2200)) +
  ggtitle("Demographic history: Habitat generalists") +
  xlab("Years ago * 10^3") +
  ylab("Effective Population Size (* 10^4)") +
  scale_colour_manual(values = colours) +
  theme_classic() +
  annotate("rect", xmin = c(14, 130), xmax = c(29, 191), ymin = -Inf, ymax = Inf,
           alpha = 1 / 5, fill = "#CDCDCD") +
  annotate("text", x = c(21, 160), y = c(100, 100),
           label = c("MIS2", "MIS6"), cex = 2.5)

# Display the scatterplot
print(p)


# Define the habitat specialist categories
habitat_specialists <- c("Forest Specialists", "Grassland Specialist")

# Filter the dataframe for habitat specialists
filtered_data2 <- combined[combined$Habitat %in% habitat_specialists, ]

# Scatterplot generation
p <- ggplot(filtered_data2, aes(x = Ya / 1000, y = Ne, group = Site.Species, col = Site.Species)) +
  geom_path() +
  scale_x_log10(limits = c(10, 2200)) +
  scale_y_continuous(limits = c(0, 100)) + 
  coord_geo(
    xlim = c(2200, 10), ylim = c(0, 100),
    dat = list("periods", "epoch" ),
    height = list(unit(4, "lines"), unit(2, "line")),
    rot = list(90, 90, 0), size = list(2.5, 2.5, 5), abbrv = FALSE
  ) +
  ggtitle("Demographic history: Habitat specialists") +
  xlab("Years ago * 10^3") +
  ylab("Effective Population Size (* 10^4)") +
  scale_colour_manual(values = colours) +
  theme_classic()
  #annotate("rect", xmin = c(14, 130), xmax = c(29, 191), ymin = -Inf, ymax = Inf,
           #alpha = 1 / 5, fill = "#CDCDCD") +
  #annotate("text", x = c(21, 160), y = c(100, 100),
           #label = c("MIS2", "MIS6"), cex = 2.5)

# Display the scatterplot
print(p)



# Define the habitat specialist categories
habitat_specialists <- c("Forest Specialists", "Grassland Specialist", "Forest Generalist", "Grassland Generalist")

# Filter the dataframe for habitat specialists
filtered_data <- combined[combined$Habitat %in% habitat_specialists, ]

# Scatterplot generation with facet wrap
p <- ggplot(filtered_data, aes(x = Ya / 1000, y = Ne, group = Site.Species, col = Site.Species)) +
  geom_path() +
  #scale_x_log10(limits = c(10, 2200)) +
  coord_geo(xlim = c(100, 0), ylim = c(10, 2200)) +
  ggtitle("Demographic history: All Habitat type") +
  xlab("Years ago * 10^3") +
  ylab("Effective Population Size (* 10^4)") +
  scale_colour_manual(values = colours) +
  theme_classic() +
  annotate("rect", xmin = c(14, 130), xmax = c(29, 191), ymin = -Inf, ymax = Inf,
           alpha = 1 / 5, fill = "#CDCDCD") +
  facet_wrap(~ Habitat, nrow = 2)  # Create separate plots for each habitat category

# Add labels for each habitat category
p <- p + labs(fill = "Species", col = "Species")

# Display the scatterplot
print(p)



