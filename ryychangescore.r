##Function to Compute Reliability of Change Scores##
##Jacobson-Truax Method
##Gulliksen-Lord-Novick Method (Adjusted for Regression to Mean)
##Edwards-Nunnally (Confidence Interval Method)
##Based on Formulae Provided by Bauer, S., Lambert, M. J., & Nielsen, S. L. (2004). Clinical significance methods: A comparison of statistical techniques. Journal of Personality Assessment, 82(1), 60-70.

##See Also:
##Jacobson NS,Follette WC,Revenstorf D. Psychotherapy outcome research: methods for reporting variability and evaluating clinical significance. Behav Ther (1984):15:336-52.
##Jacobson NS, Truax P. Clinical significance: a statistical approach to defining meaningful change in psychotherapy research. J Consult  Clin Psychol (1991):59(1):12-19.
##Hsu, L. M. (1989). Reliable changes in psychotherapy: Taking into account regression toward the mean. Behavioral Assessment, 11, 459–46
##Speer, D. C. (1992). Clinically significant change: Jacobson & Truax (1991) revisited. Journal of Consulting and Clinical Psychology, 60, 402–408.




############################
##SYNTAX EXAMPLE:          #
##ryychangescores(XT1, XT2)#
############################


#######################
#####START FUNCTION####
#######################

ryychangescores = function(x,y){

T1 = x
T2 = y

SDT1 = sd(T1, na.rm = TRUE)

Test.Retest = cor(T1, T2, use="pairwise.complete.obs", method="pearson")

Change.Score = (T2-T1)


##Jacobson & Truax Method

RC = (Change.Score)/(sqrt(2)*(SDT1*(sqrt(1-Test.Retest))))



Mean.Change.Score = mean(Change.Score, na.rm=TRUE)
Mean.RC.Score = mean(RC, na.rm = TRUE)

MDC95 = (1.96 * (sqrt(2) * SDT1*(sqrt(1-Test.Retest))))



##Gulliksen, Lord, & Novick Method (Hsu, 1989)

MT1 = mean(T1, na.rm=TRUE)
MT2 = mean(T2, na.rm=TRUE)

GLN.RCI = ((T2 - MT1) - Test.Retest*(T1 - MT1))/(SDT1*(sqrt(1-(Test.Retest^2))))
Mean.GLN.RCI.Score = mean(GLN.RCI, na.rm = TRUE)




##Edwards & Nunnally Method (Speer, 1992)
##This is a confidence Interval Method

EN.RCI.LL = (Test.Retest*(T1 - MT1)+T1) - ((2*SDT1)*(sqrt(1-Test.Retest)))
EN.RCI.UL = (Test.Retest*(T1 - MT1)+T1) + ((2*SDT1)*(sqrt(1-Test.Retest)))



Mean.EN.RCI.LL = mean(EN.RCI.LL, na.rm=TRUE)
Mean.EN.RCI.UL = mean(EN.RCI.UL, na.rm=TRUE)


print
cat("**************************************************************************************************************************************************")

print
cat("\n")

print
cat("Reliable Change (RC) is an index of the variation in change scores that that attributable to measurement error (See Jacobson 1984, 1991):", "\n")

print
cat("\n")

print
cat("For any given respondent, when RC exceeds (+/-) 1.96, it is likely (p < .05) that the change score is reflecting real change.")

print
cat("\n")

print
cat("In other words, RC tells us whether observed changes reflect more than fluctuations due to measurement (un)reliability.")

print
cat("\n")

print
cat("\n")

print
cat("Average Change Score", Mean.Change.Score, "\n")

print
cat("\n")


print
cat("Average Jacobson (1984, 1991) Reliable Change Index =", Mean.RC.Score, "\n")

print
cat("\n")

cat("Average Gulliksen, Lord, & Novick (GLN) Reliable Change Index[Hsu, 1989] =", Mean.GLN.RCI.Score, "\n")


print
cat("\n")

print
cat("MDC95 represents the expected change (e.g., XT2 - XT1) for an RC = (+/-) 1.96 (i.e., p < .05).")

print
cat("\n")

print
cat("In other words, 95% of the time, change scores that can be attributed to actual change (i.e., not measurement error) will exceed (+/-) MDC95.")

print
cat("\n")

print
cat("\n")

print
cat("MDC95 - Minimum Detectable Change at 95% Confidence Level = (+/-)", MDC95, "\n")

print
cat("\n")

cat("Edwards & Nunnally Reliable Change Index Confidence Interval [Speer, 1992]:", "\n", "Mean Lower Bound =", Mean.EN.RCI.LL, ":", "Mean Upper Bound =", Mean.EN.RCI.UL, "\n", "Mean Time 2 =", MT2)
cat("\n")
cat("\n")
cat("Note: T1 to T2 score is classified as 'reliably changed' if T2 score is outside of the confidence interval boundary (See confidence interval plot).") 

print
cat("\n")

print
cat("**************************************************************************************************************************************************")


#PLOTS

par(mfrow = c(1, 3))
hist(Change.Score, main = "Histogram of Change Scores")
hist(RC, main = "Histogram of Reliable Change (RC) Index \n [Jacobson and Truax, 1991]")
hist(GLN.RCI, main = "Histogram of GLN Reliable Change (RC) Index \n [Hsu, 1989]")


dev.new()
library(ggplot2)
data.estimates = data.frame(
  var = c('Reliable Change Index'),
  par = MT2)

data.estimates$upper <-  (Mean.EN.RCI.UL)
data.estimates$lower <-  (Mean.EN.RCI.LL)

p2 <- ggplot(data.estimates, aes(var,par, size=10)) + theme_bw(base_size=10)
p2 + geom_point() + geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=2), width = 0.2) + xlab("Confidence Interval") + ylab("Values of Confidence Interval") + ggtitle("Edwards & Nunnally Reliable Change Index Confidence Interval") + theme(legend.position = "none")


}

#######################
#####END FUNCTION######
#######################


