\name{Report}
\alias{Report}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Report(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\documentclass[10pt]{article}
\usepackage{graphicx, verbatim}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amscd}
\usepackage{lipsum}
\usepackage{blindtext}
\usepackage{todonotes}
\usepackage[tableposition=top]{caption}
\usepackage{ifthen}
\usepackage[utf8]{inputenc}
\usepackage{graphicx}
\usepackage{caption}
\setlength{\textwidth}{6.5in} 
\setlength{\textheight}{9in}
\setlength{\oddsidemargin}{0in} 
\setlength{\evensidemargin}{0in}
\setlength{\topmargin}{-1.5cm}
\setlength{\parindent}{0cm}
\usepackage{setspace}
\usepackage{float}
\usepackage{amssymb}
\usepackage[utf8]{inputenc}
\usepackage{fancyhdr}
\usepackage{tabularx}
\usepackage{soul}

\usepackage{hyperref}
\hypersetup{
  colorlinks   = true, %Colours links instead of ugly boxes
  urlcolor     = blue, %Colour for external hyperlinks
  linkcolor    = blue, %Colour of internal links
  citecolor   = red %Colour of citations
}

\usepackage[backend=biber ,sorting=none]{biblatex}
\bibliography{references}
\begin{filecontents*}{references.bib}
\end{filecontents*}


%\fancyhf{}
\rfoot{Group 2 \thepage}
\singlespacing
\usepackage[affil-it]{authblk} 
\usepackage{etoolbox}
\usepackage{lmodern}

% \makeatletter
% \renewcommand{\maketitle}{\bgroup\setlength{\parindent}{16pt}
% \begin{flushleft}
%   \textbf{\@title}
% 
%   \@author
% \end{flushleft}\egroup
% }

%\renewcommand\Authfont{\fontsize{14}{18.4}\selectfont}
%\makeatother

% \pagestyle{fancy}
% \rfoot{Page \thepage}
 %\thispagestyle{empty} 


\begin{document}


\title{\LARGE Plastic Pollution in Oceans  \\ Group 2 Report - CMM507}

\author{ALEXANDER RITCHIE, \textit{\href{1911218@rgu.ac.uk}{1911218@rgu.ac.uk}};\\ GEORGIOS ORFANAKIS, \textit{\href{1903446@rgu.ac.uk}{1903446@rgu.ac.uk}};\\ KAREN JEWELL, \textit{\href{1415410@rgu.ac.uk}{1415410@rgu.ac.uk}};\\ ROSHI SHRESTHA, \textit{\href{1903445@rgu.ac.uk}{1903445@rgu.ac.uk}};\\ STUART WATT, \textit{\href{1501869@rgu.ac.uk}{1501869@rgu.ac.uk}}}

\maketitle
% \begin{flushleft} \today \end{flushleft} 
\noindent\rule{16cm}{0.4pt}
%\underline{\hspace{3cm}
\ \\
%\thispagestyle{empty}

\section*{Objective}


\begin{itemize}
\item To understand the composition of plastic pollutants in the ocean
\item To understand the sources of plastic pollutants
\item To understand how plastic pollution gets distributed across the oceans
\end{itemize}

\section{Problem Statement}\label{statement}

H1 = The \% of plastic pollution remains constant over time.

H0 = The \% of plastic pollution does not remain constant over time.




\subsection{Overview}\label{over}

Marine pollution is a major global issue which impacts on environment, economy and human health. Although marine pollution is caused by many different materials, plastics consist of 60-80\% of the marine litter.\cite{DERRAIK2002} \cite{REISSER2013} \cite{BARBOZA2020}
\\
\\
Synthetic organic polymer derived from polymerisation of monomers extracted from oil and gas make up the plastics.\cite{DERRAIK2002} \cite{RIOS2007} The lightweight feature and its durability make it very suitable to make a range of products that we use in our everyday life.\cite{BARNES2009} \cite{SIVAN2011} These same features have been a major cause of pollution due to overuse and non-managed waste disposal system worldwide with plastic contributing to the 10\% of the waste generated worldwide.\cite{BARNES2009} Due to its buoyancy, plastic debris can be dispersed over long distances and they can persist for a long time. Although, plastic litter has been a major cause of marine pollution for a while, its seriousness has only been realised recently. Jambeck et al.,\cite{JAMBECK2015} reported that in 2010 alone, between 4.8 million to 12.7 million metric tons of plastics entered the ocean. Plastics are now everywhere in the marine environment and urgent action is required to mitigate this problem and reduce the harmful impact.\cite{RIOS2007} \cite{ROCHMAN2015}



\subsection{Motivation}\label{mot}

Impact on marine life
\\
\\
Plastics in ocean is one of the many forms of human impact that threatens marine life. There is still very little information available on the impact of plastic pollution on the ocean's ecosystem. Due to the realisation on impact of human on climate and environment, there has been a lot of awareness activities to reduce the impact of pollution. Ban on single use plastic bags are being applied to many countries in order to protect the environment. 
\\
\\
Over 700 marine wildlife species are affected due to entanglement in plastic ropes and materials and ingestion of plastics in the ocean.\cite{GALL2015} Over 340 species of marine animals were found to be entangled.\cite{KUHN2015} Reducing plastic waste is a major challenge worldwide. It is almost impossible to estimate the number of marine animals affected by marine pollution globally due to the vastness of the ocean. However, studies carried out on the gut contents of thousands of seabirds, found the significant increase in the ingestion of plastics during the 10-15 years interval.\cite{ROBARDS1995} This result might correlate to the rapid increase of plastic production and plastic use globally.  In a study carried out over fourteen years, Moser and Lee \cite{MOSER1992} found that more 50\% of the seabird species contained plastic particles in the gut which increased over time. This could be due the increase in plastic availability over time. 
\\
\\
Entanglement in plastic debris is another cause of marine life suffering. Discarded fishing gear and floating mastic masses in ocean are serious threat to marine animals. Some animals such as seals are attracted to the floating plastics where they get entangled and get suffocated. Harmful effect of litter on marine life has been reviewed extensively.\cite{GALL2015} \cite{KUHN2015} \cite{RYAN2015} \cite{WILLIAMS2019} Floating plastics over long distances can disperse alien species as well as some pathogens. Drifting plastic debris are also the source of alien species introduction and thus affecting the native marine biodiversity.\cite{GREGORY2009} \cite{KIESSLING2015} 
\\
\\
Impact on environment and human health
\\
\\
Plastic debris floating in the oceans and the littering the coastal areas are not a pleasant sight. Masses of plastic accumulation and discarded objects made from plastics are found everywhere nowadays. 
\\
\\
Over time plastic disintegrates into small microplastics which are easily consumed by fish and they enter the food chain. Plastics have been found in a third of fish caught in the UK which included the popular fishes such as cod, haddock and mackerel. Impact of plastic entering the human food chain and the effects of it are still to be studied.  Plastic toxicity and the occurrence of microplastics and nanoplastics in the water supply can also be a direct impact on human health in addition to the contamination in seafood.\cite{ROCHMAN2015} \cite{MARKIC2020}
\\
\\
Reducing plastic pollution has recently been a global aim. Research in plastic pollution in marine environment has played a big role in reducing it and raising awareness all over the world. In order to understand the plastic pollution in marine environments and its effect in long term, it is essential to keep collecting data on patterns of marine debris around the world. Effective monitoring of plastic debris is very essential in order to reduce the abundance of plastic debris everywhere. In addition, monitoring the type, frequency and the source of the litter is also important for prevention initiative of marine pollution. Most of the monitoring are done by surveys looking at frequencies of beach litter collected by organisations and volunteers.\cite{COE1997} Most abundant litter can be found close to urban areas where beach visitor numbers are higher.\cite{GARRITY1993} 






\subsection{Objectives }\label{obj}

The main objectives of this project can be outlined as follows: 



\pagebreak
\section{Research}\label{research}

Things we found

Sources of pollution: 10 river dataset, 50km2 coastline dataset, pollution density and body of water dataset....

\pagebreak
\section {Methods}\label{methods}

This paper is conducted using secondary data collection methods only. The authors did not collect or create any new data using primary methods.


\subsection{Dataset Description}\label{dataset}

\hl{data dictionary is probably better as a table than a list.}
\hl{Can we also add to this information of how the data is entered e.g. optional/mandatory fields, free-text or dropdown fields. The use of IDs suggest these are option fields with lookup tables somewhere.}

\begin{itemize}
%\Where the dataset came from; How it is constructed: multiple csv files by year; A description of what it is, what's in it and what it represents; Problems with the dataset: Missing data; data anomalies (lat/long values don't match named regions)

\begin{table}
\centering
\begin{tabular}{ c c c }
Variable & Description & Mandatory \\
\hline
ListID  & the ID code for the list & non mandatory \\
ListName & the name of the list & non mandatory \\
ItemID & ID code given to the item of debris & non mandatory \\
ItemName & name we give to item of debris & mandatory \\
LogID &  ID code given to the location of the debris & non mandatory \\
Quantity & number of pieces of debris in the observation & mandatory? \\
Error radius & radius around the observation site within the error for reasonable doubt & mandatory \\
Latitude, Longitude and Altitude & coordinates of the location where the observation was made & mandatory \\
Location & area the observation of debris was made in & non mandatory \\
Description & description of the area the debris was found in & non mandatory \\
MaterialID & ID code of the material that the debris was composed of & non mandatory \\
MaterialDescription & description of material the debris was composed of & mandatory\\ 
Time & time of observation & non mandatory \\
\end{tabular}
\end{table}

\item The data was taken from  \textit{\href{http://marinedebris.engr.uga.edu/newmap/}{marine debris tracker}} between 2010 till February 19th 2020. The time of 2010 was chosen as there was no data before that time.
\item The dataset was composed by combining the multiple csv files gathered from the marine debris tracker into a single set after this was done the date data type was renamed "Time". 
\item The dataset created from the combined csv files contain more than 360000 rows of data and consists of the folowing variables. 
  \begin{itemize}
  \item ListID is the ID code for the list
  \item ListName is the name of the list
  \item ItemID is the ID code given to the item of debris
  \item ItemName is the name we give to item of debris
  \item LogID is the ID code given to the location of the debris
  \item Latitude, Longitude and Altitude are the coordinates of the location where the observation was made
  \item Quantity is the number of pieces of debris in the observation.
  \item Error radius is the radius around the observation site within the error for reasonable doubt.
  \item Location is the area the observation of debris was made in.
  \item Description is the description of the area the debris was found in.
  \item MaterialID is the ID code of the material that the debris was composed of. 
  \item Material Description is the description given to the material that composes the debris.
  \item Time is the time that the observation was made. 
\item There were a number of problems with the dataset namely;
  \begin{itemize}
  \item There were a number of cases of missing data in the dataset. 
  \item data anomalies (lat/long values don't match named regions)
  \item 
  \end{itemize}
\end{itemize}
\end{itemize}

\subsection{Dataset Pre-processing}
%\Because of the features and concerns identified in the section above, we chose to transform the dataset in the following ways:reclassified some labels because variation was too high (there were too many labels); removed missing values; removed certain subsets; but kept certain subsets

Everything below is from Stuart's RNW file
<<eval=TRUE,echo=FALSE,error= FALSE,warning=FALSE,message=FALSE>>=
library(tidyverse)
library(purrr)
library(magrittr)
library(treemap)
library(mapdata)
library(viridis)
library(lubridate)
library(imager)


data <- list.files(path = "data/debris/", full.names = TRUE) %>% 
  lapply(FUN = read_csv, col_types = "ififidddddcfcif") %>% 
  reduce(rbind)
@


<<eval=TRUE,echo=FALSE>>=
# Data wrangling

#replace the column for time as a date data type, renaming it "Time" 
data$Time <- data$Timestamp %>% 
  parse_datetime(format = "%Y%m%d%H%M%S")
data$Timestamp <- NULL

#MissingValues
data %>% select_if(function(x) any(is.na(x))) %>% colnames()
#explicit missing value for the location factor
data$Location <- data$Location %>% fct_explicit_na()

#Remove redundant data
#Both "ListID" and "ListName" don't give us any information, so we will remove them both.
data <- data %>% select(-ListID,-ListName)
@
The following actions were performed on the dataset:

\"ListID\" and \"ListName\" were found to be redundant and removed. \hl{can we remove ItemID and MaterialID also since we have the descriptions?}\\
\\
Nulls found in ItemName and Description.\\ 
\hl{this means every entry has a material at least? why? could be a required field? that would explain why some entries are rubbish if people are forced to pick a category}
\hl{it is also worth discussing the merits of dropdown entries: standardises input but forces a value where none might be appropriate, or a default it selected?}
\\
\hl{Stuart: Yes, it is a required field. I checked on the mobile app and you select a item type from different material sections. Note however that there is a material type \emph{Other Items} which contains the items \emph{Other} and \emph{Test Item}. Therefore users are able to categorise an item as other if it is not appropriate for any other option on the list.}
\\
NAs removed \hl{not sure what is actually happening with the explicit\_na() piece for location?}\\
\\
\hl{Stuart: I used explicit\_na to replace missing values with an expliciet level name (like ``Missing'' or something similar). I think it was to remove warnings when I was plotting.}
\\
\hl{also maybe worth looking at: what's the significance of some of these itemIDs where the itemname is blank? It could be an item once that was then deleted or categorised retrospectively. Do a groupby ItemID and see if more than one material or item name turns up.}\\
\\
\hl{I was puzzled by this so I investigated it and found it is not in the source data. I found a coding bug further down in the code, where I make the stacked bar chart with proportions per month. I have fixed this, and now no ItemName values are blank. Sorry!}
\\
Unique values for each column: \hl{can we present these unique counts as a formatted table? I think it's interesting that 55 unique items can have \~8k descriptions}
<<eval=TRUE,echo=FALSE,results='markup'>>=
data %>% apply(2, function(x) length(unique(x)))
@

\subsection{Data Quality Issues: Classification}
The authors find that there are multiple instances of missclassified items. Where their descriptions appear to not match their material categorisation\\
Lets see if there are any "ItemNames" associated with more than one "Material Descriptions".
<<eval=TRUE,echo=FALSE,results='markup'>>=
data %>% select(`Material Description`, ItemName) %>%
  distinct() %$% 
  table(ItemName) %>% 
  as_tibble() %>% 
  filter(n > 1)
@
So rubber gloves are associated with two material descriptions, but otherwise a one to many relationship exists between "Material Description" and "ItemName".
<<eval=TRUE,echo=FALSE,results='markup'>>=
data %>% select(`Material Description`, ItemName, Quantity) %>%
  filter(ItemName == "Rubber Gloves") %>% 
  group_by(`Material Description`) %>% 
  summarise(Quantity = sum(Quantity))
@
It seems that most rubber gloves are classified as plastic rather than rubber.
<<eval=TRUE,echo=FALSE,results='markup'>>=
data %>% select(`Material Description`, ItemName, Description) %>%
  filter(ItemName == "Rubber Gloves", !is.na(Description))
@
All instances of rubber gloves with non-missing descriptions are categorised as plastic. We also see that the descriptions suggest that the categorisation may be innaccurate: the last two instances here have "Balloon" in the extra descriptions... why aren't they categorised as such?
\hl{another thing maybe worth looking at: all MATERIALS!=Plastic yet have the term "plastic" in the description. could further expand this to descriptions which have any of the material terms in them, but is not its own material. further explores the point about missclassified data.}





\subsection{Recategorisation}


After the issues with the dataset that were identified in the section above, it was decided that it would be best to transform the dataset in the following ways:
\begin{itemize}
\item reclassified some labels because variation was too high (there were too many labels)
\item The values of the missing data were removed.
\item It was decided that subsets that were not needed were removed while retaining the necessary subsets.
\end{itemize}

\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE,fig=TRUE>>=
plastic_ordered <- data %>% 
  filter(`Material Description` == "PLASTIC") %>% 
  select(ItemName, Quantity) %>% 
  group_by(ItemName) %>% 
  summarise(Total = sum(Quantity)) %>% 
  arrange(desc(Total))

data %>% 
  filter(`Material Description` == "PLASTIC") %>% 
  mutate(month = month(Time, label = TRUE), 
         year = as.integer(year(Time)),
         ItemName = fct_infreq(ItemName)) %>% 
  filter(year > 2010) %>% 
  group_by(month, year, ItemName) %>%
  summarise(`Total Quantity` = sum(Quantity)) %>% 
  ggplot(aes(x = month, y = `Total Quantity`, fill = ItemName)) +
    geom_col(colour = "black", size = 0.2, position = "fill") +
    facet_wrap(~year, nrow = 4) +
    scale_fill_hue(l=50, c=150) +
    xlab("Month") +
    ylab("Proportion of Items") +
    theme(legend.position="top")
    #scale_fill_viridis_d(option = "magma")

#ggsave("plots/pastic_debris_plot.png", width = 40, height = 20, units = "cm")
@
\caption {Debris by categorisation}
\label{figE}
\end {center}
\end {figure}





\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE,fig=TRUE>>=  
# all cigarette related waste: 1, 4, 6, 22
# Food related waste: 3, 2,7,9,10, 17, 23, 11
# Non food related waste: 8, 14, 15, 16, 18, 19, 21, 20
# Plastic bags and Styrofoam packaging:12, 13
# Fragments: 5, 23, 24,25

recategorise <- function(x){
  out = ""
  if(x %in% c(1,4,6,22)){out = "Cigarette related waste"}
  if(x %in% c(2,3,7,9,10,17,23,11)) out = "Food related waste"
  if(x %in% c(8,14,15,16,18,19,21,20)) out = "Other"
  if(x %in% c(12,13)) out = "Plastic bags and Styrofoam packaging"
  if(x %in% c(5,23,24,25)) out = "Fragments"
  if(out == "") stop(paste("Error in recategorise:", x))
  return(out)
}

plastic_types <- data %>% 
  filter(`Material Description` == "PLASTIC") %>% 
  select(ItemName, ItemID) %>% 
  distinct() %>% 
  mutate(label = 1:n()) %>% 
  mutate(category = purrr::map(label, recategorise)) %>%
  mutate(category = as_factor(as.character(category))) %>% 
  select(ItemID, category)


plastic <- data %>% 
  filter(`Material Description` == "PLASTIC") %>% 
  full_join(plastic_types, by = "ItemID")

plastic %>% 
  mutate(month = month(Time, label = FALSE), 
         year = as.integer(year(Time))) %>% 
  filter(year > 2010) %>% 
  group_by(month, year, category) %>%
  summarise(`Total Quantity` = sum(Quantity)) %>% 


#Recategorisation by year
    ggplot(aes(x = month, y = `Total Quantity`, fill = category)) +
    geom_col(colour = "black", size = 0.2, position = "fill") +
    facet_wrap(~year, nrow = 4) +
    scale_fill_viridis(discrete = TRUE, option = "plasma") +
    xlab("Month") +
    ylab("Proportion of Items") +
    ggtitle("Rel. frequencies of observed plastic waste by category") +
    scale_x_continuous(breaks = 1:12) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position="top") +
    guides(fill=guide_legend(title="Category"))

#ggsave("plots/pastic_debris_plot_recategorised.png", width = 40, height = 20, units = "cm")

@
\caption {Recategorisation by year}
\label{figB}
\end {center}
\end {figure}



\pagebreak
\section{Exploration}

Here we describe the things we found... 

\subsection{Proportion Trends}
How pollutant proportions change over time.\\
\\
Cigarette butts proportions and raw counts decrease over time: possibly less people smoking, or moving to vaping\\
\\
General pollution count going down over time?\\
\\
Old pollutants fall away (cigarette butts) but new ones are introduced\\
\\
Question: Are observed plastic item proportions time invariant?\\
\\
\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE, fig=TRUE>>=
#Linechart quantity of debris per year
data %>% 
  mutate(year = year(Time)) %>% 
  filter(year > 2010, year < 2020) %>% 
  group_by(year) %>% 
  summarise(quan = sum(Quantity)) %>% 
  ggplot(aes(x = year, y = quan)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 2011:2019) +
    xlab("Year") +
    ylab("Quantity") +
    ggtitle("Quantity of debris per year") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

#ggsave("plots/observations.png")
@
\caption {Trend of debris observered}
\label{figD}
\end {center}
\end {figure}
\hl{this chart needs to size down}



\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE,fig=TRUE>>=
#Histogram of observations: Total v Plastic
data %>% 
  mutate(Type = if_else(`Material Description` == "PLASTIC", "Plastic", "Other"),
         months = floor_date(Time, 'month')) %>% 
  group_by(months, Type) %>% 
  summarize(`Number of observations` = n()) %>% 
  ggplot(aes(x = months, y = `Number of observations`)) +
    geom_area(aes(fill = Type))
  # ggplot() +
  # geom_histogram(aes(x = Time))
  
@
\caption {Observations of plastic debris v all debris}
\label{figF}
\end {center}
\end {figure}
\hl{this chart needs to size down}



\subsection {Distribution of observed debris:}

MaterialQuantities\\

\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE,fig=TRUE>>=
data %>% select(Quantity,Description,`Material Description`) %>% 
  group_by(`Material Description`) %>% 
  summarise(Quantity = sum(Quantity)) %>% 

  ggplot(aes(x = reorder(`Material Description`, Quantity), y = Quantity)) +
    geom_col() +
    ylab("Total recorded quantity") +
    xlab("Material class") +
    coord_flip()
@
\caption {Material Quantities}
\label{figC}
\end {center}
\end {figure}

So the most populated material class is Plastic. Note that this does not necessarily mean that plastic is the largest quantity of debris, just that the individual number of items categorised is largest.

A tree map of material quantities:
\begin{figure}[H] %start a figure
\begin{center}
<<eval=FALSE,echo=FALSE,fig=TRUE,warning=TRUE,error=TRUE>>=
#treemap of debris categories
#png("plots/treemap.png")
data %>% 
  select(`Material Description`, ItemName, Quantity) %>% 
  group_by(`Material Description`, ItemName) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  treemap(index = c("Material Description", "ItemName"),
          vSize = "Quantity", draw = TRUE) -> tm
#tm
#dev.off()
#save.image(file = "plots/treemap.png")
@
\caption {Debris categorisation}
\label{figI}
\end {center}
\end {figure}
\hl{chart disabled for now. This chart was working for a while (works in RStudio R, not LaTeX) only hint I can see is:\#\# Warning: Factor `ItemName` contains implicit NA, consider using `forcats::fct\_explicit\_na`}

Cigarettes are the most common item recorded as seen in. %Figure~\ref{figI}
Perhaps some of the debris is not actually from the sea, but rather from people littering by the coastline? Does debris littered on the coastline end up in the oceans?

\hl{This is a great chart, but not the best to support the statement that cigarettes is most popular - a column or bar chart here will be much better (area charts are not as effective as charts you can level-compare), potentially use proportions or data labels to further drive the point that it IS the largest. Treemap suggest moving back into pre-processing section.}



\subsection{Event-Driven Pollution}

Fireworks found in July and North-America only: possibly 4th July celebrations\\
4th July and Firework link? (Karen's Idea)\\

\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE,fig=TRUE>>=
#Boxplot of fireworks distribution by month (across all years)
data %>% 
  filter(`Material Description` == "PLASTIC",
         ItemName %in% c("Fireworks"),
         year(Time) >= 2012, year(Time) <= 2019) %>% 
  mutate(month = month(Time, label = TRUE), 
         year = as.integer(year(Time))) %>% 
  group_by(month, year) %>% 
  summarise(quantity = sum(Quantity)) %>% 
  ggplot() +
    geom_boxplot(aes(x = month, y = quantity)) +
    xlab("Month") +
    ylab("Quantity") +
    ggtitle("Firework debris 2012-2019")

#ggsave("plots/fireworks.png")
@

\caption {Boxplot of fireworks distribution by month, across all years}
\label{figA}
\end {center}
\end {figure}
\hl{this chart needs to size down}





\subsection{Location-Driven Pollution}

Rubber found in Indoneasia only: possibly a recording bias.\\

Certain classes are found in certain regions only: not because they don't exist elsewhere but because of recording bias focus in those areas\\

We have locational data, so lets check for any geographical observation bias.
<<eval=TRUE,echo=TRUE>>=
world <- map_data("world")
data %>% 
  select(Latitude, Longitude, Quantity, Location, `Material Description`) %>% 
  ggplot() +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) +
    geom_hex(aes(x = Longitude, y = Latitude), bins = 50) +
    scale_fill_viridis(trans = "log", breaks = c(5, 50, 500, 5000, 50000)) +
    theme_void() +
    guides(fill=guide_legend(title="Observations"))

#ggsave("plots/map.png", width = 20, height = 10, units = "cm")
@
There seems to be a strong bias towards North America in our dataset. We will try a logarithmic plot to see things more clearly:
<<eval=TRUE,echo=TRUE>>=
data %>% 
  select(Latitude, Longitude, Quantity, Location, `Material Description`) %>% 
  ggplot() +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) +
    geom_hex(aes(x = Longitude, y = Latitude, fill = stat(log(count))), bins = 50) +
    scale_fill_viridis() +
    theme_void()
@

We need to know how reliable the location data is. I'm going to filter for "united kingdom" in the location field and plot the raw coordinates.\\

\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=TRUE,fig=TRUE>>=
#Scatterplot of long/lat positions
data %>% 
  mutate(Location = str_to_lower(Location)) %>% 
  filter(str_detect(Location, "united kingdom")) %>% 
  select(Latitude, Longitude) %>% 
  ggplot(aes(x = Latitude, y = Longitude)) +
    geom_point(position = "jitter")
@
\caption {Longitude and Latitude discrepancies}
\label{figH}
\end {center}
\end {figure}
\hl{this chart needs to size down}
We have a outliers here. Maybe a difference in standards used for Longitude and Latitude? Some systems put the Latitude origin close to the UK.\\

Questions\\
Distribution of plastic by location.\\
Are the distributions of plastic fairly constant for the locations with the most observations?\\
\begin{figure}[H] %start a figure
\begin{center}
<<eval=TRUE,echo=FALSE,fig=TRUE>>=
#columnchart of debris locations
topLocations <- data %>% 
  group_by(Location) %>% 
  summarise(sumQuantity = sum(Quantity)) %>% 
  arrange(desc(sumQuantity)) %>% 
  top_n(5,sumQuantity)

data %>% 
  filter(Location %in% topLocations$Location) %>% 
  group_by(Location, `Material Description`) %>% 
  summarise(sumQuantity = sum(Quantity)) %>% 
  arrange(desc(sumQuantity)) %>% 
  ggplot(aes(x = `Material Description`, y = sumQuantity, fill = Location)) +
    geom_col() + theme(legend.position = 'top')
@
\caption {Debris by location}
\label{figG}
\end {center}
\end {figure}
We see that the Location "unknown" has the most plastic... note that this is distinct from "(Missing)", which was our original NA values. Maybe we should merge these.
\hl{this chart needs to size down}



\subsection{Item Pairing} 
(e.g. are 6-pack beer rings observed at the same time as fireworks? )
\hl{are we going to explore this one?}

\pagebreak
\section{Predictive Modelling}
The authors of this report built a model to predict the proportion of plastics given Month and Location. This would give more accurate predictions as opposed to a simple linear model, given we know that event-driven pollution will determine different pollutants are different times.

\subsection{Description of Model}

Georgios' script
<<eval=FALSE,echo=TRUE>>=

plasticN <- plastic %>% 
  mutate(year = as.integer(year(Time))) %>% 
  filter(year > 2010) %>% 
  group_by(year, category) %>%
  summarise(`Total Quantity` = sum(Quantity))
  
  ####
library(dplyr)
df11N <- plasticN  %>%
  filter(year == 2011) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))

df12N <- plasticN  %>%
  filter(year == 2012) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))


df13N <- plasticN  %>%
  filter(year == 2013) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))


df14N <- plasticN  %>%
  filter(year == 2014) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))

df15N <- plasticN  %>%
  filter(year == 2015) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))


df16N <- plasticN  %>%
  filter(year == 2016) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))



df17N <- plasticN  %>%
  filter(year == 2017) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`))


df18N <- plasticN  %>%
  filter(year == 2018) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`)) 

df19N <- plasticN  %>%
  filter(year == 2019) %>% 
  group_by(year) %>%
  mutate(freq = `Total Quantity` / sum(`Total Quantity`)) 


dfTotN <- rbind(df11N, df12N, df13N, df14N, df15N, df16N, df17N, df18N, df19N)

# plot for observing the data
(time_plotfr2N <- ggplot(dfTotN, aes(x = year, y = freq, color=category, fill = category)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("Years") +
  ylab("freq") +
  ggtitle("portion of plastic") + 
  expand_limits(y=0) +
  scale_y_continuous() + 
  scale_x_continuous()+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size=5, face="bold")))
  
  ### MODELING with new categorisation


# create train and test set
n <- nrow(dfTotN)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_dfTotN <- dfTotN[tindex,]   # Create training set
test_dfTotN <- dfTotN[-tindex,]

#  Pr(>|t|) is the p-value, defined as the probability of observing any value equal or larger than t if H0 is true. The larger the t statistic, the smaller the p-value. Generally, we use 0.05 as the cutoff for significance; when p-values are smaller than 0.05, we reject H0. Here p is pretty big which means that there is statistically significant correlation between relative frequency and years passing by. Which basically further supports our initial hypothesis in this project. I have included a prediction on the test set but it is of no worth obviously.


# linear model on train set
print("train model")
set.seed(1234)
dfTot_train.modelN <- lm(freq ~ year, data = train_dfTotN)
summary(dfTot_train.modelN)

# plotting frequencies according to train data
ggplot(data = train_dfTotN, aes(x = year, y = freq)) +
geom_point() +
stat_smooth(method = "lm", col = "dodgerblue3") +
theme(panel.background = element_rect(fill = "white"),
axis.line.x=element_line(),
axis.line.y=element_line()) +
ggtitle("Linear Model Fitted to Data")


print("PREDICTION")
predN <- predict(dfTot_train.modelN, test_dfTotN)
summary(predN)

# make actuals_predicteds dataframe
actuals_preds <- data.frame(cbind(actuals=test_dfTotN$freq, predicteds=predN)) 
head(actuals_preds)
# A simple correlation between the actuals and predicted values can be used as a form of accuracy measure. A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, i.e. when the actuals values increase the predicteds also increase and vice-versa.

correlation_accuracy <- cor(actuals_preds)  # 5.31%
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 53.73%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 99.4%, mean absolute percentage deviation
# Intrestingly enough min_max accuracy and mostly mean absolute percentage deviation score quite well
# but still on a model that can not be trusted.

@

\subsection{Model Evaluation}


\subsection{Model Results}
Time does not impact plastic composition.

\pagebreak
\section{Discussion}
It was noted that the main system for reporting debris was used by large scale clobes. This means that data is not a continuous and even flow so during events such as international beach cleanup day there may be more data in the respective month. Since these events aim mostly to cleanup after big social events extra effort might have been made to retrive entertainment based debris such as fireworks, food packaging and six pack rings.


\section{Conclusion and Future Work}\label{cdsmote1}

Our hypothesis stands/does not stand.


%Edit here 
%\blindtext[2]




\pagebreak
\section{Project Management}\label{mgt}
\subsection{Facilities}
Group 2 communicated using a dedicated Slack Channel, Github repository and weekly 1 hour meetings before the wednesday lab.
All project documents used and the final report can be accessed from the \textit{\href{https://github.com/KarenJewell/CMM507Group2}{Public Github Repository}}
\hl{obviously we need to mention the whole covid-19 thing and how we worked around it.}

\subsection{Project Progress}

% Pay attention to the code below including the chunk options 
<<eval=TRUE,echo=FALSE,warning=FALSE,message=FALSE,results='asis'>>=
require(openxlsx);
require(readxl)
library(stringr);library(data.table)
library(XLConnect)
library(xtable)

# Sheets names 
fileName <- 'data/meetings.xlsx'
sheets <- readxl::excel_sheets(fileName)
#length(sheets)
# Read 1st sheet (you shouldn't have more than one sheet for this task)
# read sheet into dataframe, and rbind
dfs <- readWorksheet(loadWorkbook("data/meetings.xlsx"),sheet=1)
dfs$Date <-as.character(dfs$Date)
print(xtable(dfs,
                    caption = "Record of Team Meetings", 
                    label = "tab:one", 
                    table.placement = "", 
                    # align changes subject to number of columns 
                    align = "lllp{8cm}lllll"),include.rownames=FALSE,
                    caption.placement = "top")

    
@




\subsection{Peer-assessment}

<<eval=TRUE,echo=FALSE,warning=FALSE,message=FALSE,results='asis'>>=
require(openxlsx);
require(readxl)
library(stringr);library(data.table)
library(XLConnect)
library(xtable)

# Sheets names 
fileName <- 'data/peers.xlsx'
sheets <- readxl::excel_sheets(fileName)
#length(sheets)
# Read 1st sheet (you shouldn't have more than one sheet for this task)
# read sheet into dataframe, and rbind
dfs <- readWorksheet(loadWorkbook("data/peers.xlsx"),sheet=1)

# convert fields into chars
dfs[, ] <- lapply(dfs[, ], as.character)

print(xtable(dfs,
                    caption = "Peer Assessment out of 100", 
                    label = "tab:two", 
                    table.placement = "", 
                    # align changes subject to number of columns 
                    align = "lllllll"),include.rownames=FALSE,
                    caption.placement = "top")

    
@


\pagebreak
\section*{References}\label{pubs}
\printbibliography[heading =none]

\end{document}