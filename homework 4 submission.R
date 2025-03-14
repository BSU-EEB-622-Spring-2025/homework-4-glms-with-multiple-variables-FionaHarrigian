## Homework 4 Submission ## 

#load data
mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)
str(mistletoe)
mistletoe$Year<-as.factor(mistletoe$Year)
#load packages
library(performance)
library(marginaleffects)
## Question 1:

## 1a)Fit a glm assessing evidence for the following
#hypothesis: Seedling density is increased beneath trees experiencing
#mistletoe infection. Describe the rationale you used in selecting a glm
#structure and probability distribution. Calculate model fit using MAE.
hist(mistletoe$Seedlings)
#use poisson distribution because the data is counts of seedlings
mistle.mod <-glm(Seedlings~Treatment, data=mistletoe, family = "poisson"(link = "log"))
summary(mistle.mod)
# The performance package contains great functions for MAE/RMSE (rather than always writing your own :))
performance::mae(mistle.mod) #145.841

## 1b)Use visual (e.g. a marginal effects plot) and 
#written (e.g. effect sizes on scale of response) approaches to interpret
#the results of your model. 
#These steps allow me to see the plot of the parameter estimate for seedling density
#and the estimates for seedling density in each group (parasitized and unparasitized).
plot_predictions(mistle.mod, condition="Treatment")
predictions(mistle.mod, newdata=data.frame(Treatment=c("parasitized", "unparasitized")))

## What is the difference in seedling density between parasitized and unparasitized trees?
marginaleffects::comparisons(mistle.mod, condition="Treatment", newdata=data.frame(Treatment=c("parasitized")))

#Based on your fitted model results and model fit, 
#write 3-4 sentences to discuss the following biolgical
#conclusions:  
#  Does mistletoe infection alter seedling density? How much does seedling
#recruitment differ beneath parasitized and unparasitized trees? Explain
#which elements of your glm results informed your conclusions and annotate the
#steps you needed to take to interpret your parameters. 

#Mistletoe infection increases seedling density remarkably. Parasitized trees
#recruit 295 more seedlings than unparasitized trees, a significantly larger number
#of seedlings (p<0.001). However, the MAE is 145.841, so the model does not do an
#excellent job at predicting the number of seedlings and that they might be very
#different when the tree is unparasitized versus parasitized.

## 1c) During the course of this study, 2012 was an atypically
#rainy year, compared to 2011. Fit an additional glm that quantifies how
#the effect of mistletoe differs between the two years in this study.
#Write ~2 new sentences that summarize the results of the new model and their
#biological implications.
yearmistle.mod <- glm(Seedlings~Treatment + Year, data=mistletoe, family = poisson(link = "log"))
summary(yearmistle.mod)

#try these instead of below
plot_predictions(yearmistle.mod, condition=c("Year", "Treatment"))
predictions(yearmistle.mod, newdata=data.frame(Year=c("2011", "2012"),
                                         Treatment=c("parasitized")))
predictions(yearmistle.mod, newdata=data.frame(Year=c("2011", "2012"),
                                         Treatment=c("unparasitized")))
#When year is included in the model, there is an interaction between the trees
#being parasitized and the year in which the seedling density data was collected.
#The difference in seedling density under parasitized and unparasitized trees was 
#205 for 2011 and much larger -384 for 2012. Because 2012 was a much rainier year,
#and parasitized trees were more likely to have larger areas of their canopy open,
#perhaps the rain could reach the seedlings recruited to these parasitized trees
#much more easily, meaning a much higher density of seedlings.


## Question 2:Questions 2 uses the “treemortality” dataset in this repository. Forest
#thinning is a forest management approach that aims to remove woody fuels
#from forests before wildfires to reduce subsequent severity of these
#disturbances. This data set examines the effects of a forest thinning
#treatment on tree mortality in a subsequent wildfire in forests in the
#Sierra Nevada mountains.

#In 2019, researchers measured the diameters of \>10,000 Ponderosa pine
#trees. Following this initial survey, some of the sampled areas happened
#to receive thinning treatments, applied by the US Forest Service in
#2020-2022, in a fashion unrelated to the researchers’ original study
#design. In 2023, a large portion of their sampled area then burned in a
#wildfire, creating a “natural experiment.”

#In Fall 2023, the researchers returned to re-sample survival 1000 of
#these trees, to determine whether past thinning actions helped reduce
#tree mortality (0=survived; 1=died). They recorded mortality, tree size
#(cm in diameter), and whether the tree was located in an area that had
#received a thinning treatment (**thinning**, a categorical variable,
#where 1 indicates that the plot received a thinning treatment). Due to
#the observational nature of their study, the researchers were worried
#about possible confounding relationships generated by the fact that: a)
#thinning treatments were more likely to occur in stands with lots of
#small trees, rather than larger diameter trees, and b) larger trees are
#more likely to survive fire. Therefore, they resampled the 1000 trees in
#a randomized fashion by tree size, to ensure that small and large stems
#were recorded equally commonly across thinned and unthinned forests.

#The researchers are interested in the following question: Does thinning
#decrease the probability of tree mortality in wildfire?
treemortality <- read.csv("treemortality.csv")
head(treemortality)
hist(treemortality$mortality)

## 2a) Fit a glm (using a probability distribution of your
#choice) that reflects the following research question (including thinning
#as your only predictor and mortality as your response): Do forest
#thinning treatments reduce the probability of tree mortality? Interpret
#the results of the glm by writing 2-4 sentences about the biological
#significance of the effect of thinning, including descriptions of the
#sizes of the effect on the scale of the response variable, evidence
#for/against the hypothesis, and a metric of model fit.
thin.mod <-glm(mortality~thinning, data=treemortality, family = "binomial"(link = "logit"))
summary(thin.mod)

#MAE from performance package
performance::mae(thin.mod)
#0.4080684

#plot of effects of thinning, not thinning, and parameter estimates
plot_predictions(thin.mod, condition="thinning")
predictions(thin.mod, newdata=data.frame(thinning=c(0,1)))

## What is the difference in seedling density between parasitized and unparasitized trees?
marginaleffects::comparisons(thin.mod, condition="thinning", newdata=data.frame(thinning=c(1)))

#Forest thinning treatments significantly reduce tree mortality (p<0.001). Without
#thinning, trees have 0.433 higher probability of dying during a fire than with 
#thinning. Even though trees in thinned areas still have a 0.297 probability of 
#dying, this reduction in mortality by more than half (0.730 to 0.297) is quite
#important, especially for trees which are literally rooted in place and must avoid 
#fire simply by a reduction in the potential fuel surrounding them. However, the MAE
#of the model is 0.408, so it is not a very effective model to predict accurately
#if trees would more likely die in thinned or unthinned areas because predicted 
#probabilities would be "off" by 0.4 when the probabilities for unthinned is 0.730
#and thinned is 0.297.

## 2b)The researchers explicitly considered the potential for
#confounding relationships related to tree size in their design and
#randomized their post-fire sampling by tree size. Given this
#information, do the researchers need to incorporate tree size into their
#glm to accurately estimate the effect of thinning? Why or why not?
thinsize.mod <-glm(mortality~thinning+treesize, data=treemortality, family = "binomial"(link = "logit"))
summary(thinsize.mod)
plot_predictions(thinsize.mod, condition=c("treesize", "thinning")) #as tree size increases, mortality decreases
predictions(thinsize.mod, newdata = data.frame(thinning=c(0,1),
                                               treesize=mean(treemortality$treesize)))
predictions(thinsize.mod, newdata = data.frame(thinning=1, treesize=c(0.01, 19.9)))
predictions(thinsize.mod, newdata = data.frame(thinning=1, treesize=c(0.1, 19.9)))
predictions(thinsize.mod, newdata = data.frame(thinning=1, treesize=c(1, 19.9)))
#Yes because tree size has a significant effect on mortality whether areas have
#been thinned or not. When the range of tree sizes are used to predict probability
#of mortality, larger trees with diameters about 19 cm in thinned areas had mortality
#probabilities half of small trees with diameters about 1 cm.

## 2c) The researchers submit their analysis for peer review,
#and one of the reviewers raises some concerns about the sampling
#methods. The researchers were unable to control the placement of forest
#thinning treatments (which are controlled by a variety of ecological,
#logistical, and political factors), so the reviewer is concerned about
#confounding relationships that might bias the thinning effect.

#In the reviewer’s experience, thinning treatments are not randomly
#applied across the landscape, and tend to occur in places that are
#easier to access with the heavy machinery required for thinning. For
#instance, steeper slopes might be less likely to be thinned, and they
#also tend to foster higher fire severities; similarly, forest patches
#that are farther from the road may also be less likely to be thinned and
#also experience higher fire severities, because they are farther away
#from firefighting/fire suppression activities the occur more commonly
#along roads. The reviewer sends the researchers the DAG below to
#summarize their concerns and asks the researchers to fit a new model
#that accounts for these confounding relationships. The researchers
#calculate the slope of the terrain around each sampled tree and how far
#each tree was from the nearest road, and add these variables (roaddist
#(in km), and slope (in degrees)) to their dataset.

#Refit the model from 2a to include the necessary variables to minimize
#bias in our estimation of the “thinning” variable, based on the
#reviewer’s proposed DAG (above). Does the effect of “thinning” change?
#If so, describe the degree of change and why the two models may differ
#in their conclusions. If needed, modify your model interpretation from 2a.

#*(It is not required to include any biological/ecological hypotheses
#  here that require additional or deeper knowledge about forest fires,
#  thinning treatments, etc – assume that the DAG proposed is complete.
#  Though feel free to come up with a biological explanation too, if you’d
#  like! …and feel free to scale variables where you find it useful!)*
thinslopedist.mod <- glm(mortality~thinning+roaddist+slope, data=treemortality, family = binomial(link = "logit"))
summary(thinslopedist.mod)

#predictions with new model
plot_predictions(thinslopedist.mod, condition=c("slope", "roaddist", "thinning"))
plot_predictions(thinslopedist.mod, condition=c("roaddist", "slope", "thinning"))
#different configurations of the condition argument give different plots
predictions(thinslopedist.mod, newdata=data.frame(thinning=c(0, 1),
                                                  roaddist=mean(treemortality$roaddist),
                                                  slope=mean(treemortality$slope)))

performance::mae(thinslopedist.mod) #0.161522-better fit than in 2a

#Yes, the effect of thinning shrinks. Now, the probability of mortality for trees 
#in unthinned areas is 0.529 compared to 0.730. For thinned areas, it is now 0.310,
#slightly higher chance of death compared to 0.297. The two models differ because
#the more simple one does not include the forks present in the model with more
#predictors. The distance from road and slope parameters effect both whether the
#area is thinned or not and the probability of mortality of the tree in that area. 
#When those two predictors are included, they give a more accurate depiction of 
#the effect of thinning on tree mortality. Interestingly, the fit of the model
#is better too, with a MAE of 0.16 compared to 0.4. This suggests that adding
#these parameters was quite impactful. 
