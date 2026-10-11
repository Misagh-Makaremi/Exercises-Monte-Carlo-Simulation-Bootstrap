### EXERCISE 1 ###
treatment=c(94,197,16,36,99,141,23)
control=c(52,104,146,10,50,31,40,27,46)
###############################
t.test
t.test(treatment, control, var.equal = TRUE, conf.level = 0.95)
############################
var.test(treatment,control)
############################
kruskal.test(list(treatment, control))
###########################
shapiro.test(treatment)
####################
shapiro.test(control)
####################
qqnorm(treatment)
qqline(treatment)
################
qqnorm(control)
qqline(control)
###############
hist(treatment, breaks = "FD", freq = FALSE)
hist(control, breaks = "FD", freq = FALSE)
