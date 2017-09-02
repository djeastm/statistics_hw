## ENTER DATA
lines = readline(prompt="How many lines of data? ")
data_str = readline(prompt="Please enter data as a vector with spaces separating each entry: ")
data1 = as.numeric(unlist(strsplit(data_str," ")))

if (as.character(lines) == "2") {
  data_str = readline(prompt="Please enter data as a vector with spaces separating each entry: ")
  data2 = as.numeric(unlist(strsplit(data_str," ")))
}

## FOR TESTING ONLY: TAKE OUT!!

# t-Test
#data1 = c(91.6,95.9,114.5,93.6,112.7,93.6,99.2,93.2,105.9,110.8,127.0,91.5,100.5,104.9,104.8,124.1,124.6,109.1,98.7,95.8)
#data2 = c(96.6,97.5,113.9,88.9,108.3,99.0,97.3,87.5,96.8,102.1,114.5,100.9,100.0,100.7,99.4,117.1,111.3,102.2,95.3,91.1)

# Sign Test
#data1 = c(19.7,14.7,19.2,15.1,10.4,12.1,13.1,12.1,19.6,33.3)
#data2 = c()
paired = "paired"

# Both
test_type = "2"
null_hyp = 0
#"If there is _______, we will see our data or more extreme...:"
null_hyp_expl_str = "no mean difference between standing and supine blood pressure"
alt_hyp_type = "\\neq"
#"There is [evidence measure] that ___________ (p = X.XXXX): "
alt_hyp_expl_str = "average blood pressure varies based on position"

## FOR TESTING ONLY: TAKE OUT!!

# TYPE OF TES
##cat("Test types:\n")
##cat("1) One Sample t-Test\n")
##cat("2) Sign Test\n")
##test_type <- readline(prompt="What type of test?: ")

if (as.character(test_type) == "1") {
  var = "\\mu"
  if (length(data2) > 0) {
    cat("Making data into 1 vector for this test.")
    data1 = data2-data1
  }
} else if (as.character(test_type) == "2") {
  var = "\\theta"
  ##paired <- readline(prompt="Is the data paired or independent?: ")
}


## STATE HYPOTHESES
# NULL HYPOTHESIS
##null_hyp <- as.numeric(readline(prompt="Enter null hypothesis (H_o): "))
##null_hyp_expl_str <- readline(prompt="If there is _______, we will see our data or more extreme...: ")

null_hyp_str = paste("$$H_0:",var,"=",null_hyp,"$$")


# ALTERNATIVE HYPOTHESIS
##alt_hyp_type <- readline(prompt="Enter alternative hypothesis operator: ")
##alt_hyp_expl_str <- readline(prompt="There is [evidence measure] that ___________ (p = X.XXXX): ")

alt_hyp_str = paste("$$H_A:",var,alt_hyp_type,null_hyp,"$$")

if (as.character(test_type) == "1") { # in One-Sample t-Test Mode
  
  ## CALCULATE TEST STATISTIC
  x_bar = round(mean(data1),digits=6)
  mu_0 = null_hyp
  sd = round(sd(data1),digits=6)
  n = length(data1)
  test_stat = round((x_bar-mu_0)/(sd/sqrt(n)),digits=6)
  deg_free = n-1
  abstract_str = "\\frac{PE-H_0}{SE}"
  prototype_str = "\\frac{\\overline{x}-\\mu_0}{\\frac{s}{\\sqrt{n}}}"
  actual_str = paste("\\frac{",x_bar,"-",mu_0,"}{\\frac{",sd,"}{\\sqrt{",n,"}}}",sep="")
  calc_test_stat_str = paste("$$T = ",abstract_str," = ",prototype_str," = ",actual_str," = ",test_stat,"$$",sep="")
  
  ## CALCULATE P-VALUE
  
  if (alt_hyp_type == "<") {
    p_val = pt(test_stat,deg_free)
    p_call_str = paste("\\verb|pt(T,df) = pt(",test_stat,",",deg_free,")|",sep="")
    alter_param = "less"
  } else if (alt_hyp_type == ">") {
    p_val = 1-pt(test_stat,deg_free)
    p_call_str = paste("\\verb|1-pt(T,df) = 1-pt(",test_stat,",",deg_free,")|",sep="")
    alter_param = "greater"
  } else { # \neq
    p_val = 2*(1-pt(abs(test_stat),deg_free))
    p_call_str = paste("\\verb|2*pt(abs(T),df)) = 2*pt(abs(",test_stat,"),",deg_free,")|",sep="")
    alter_param = "two.sided"
  }
  
} else if (as.character(test_type) == "2") {# in Sign Test Mode
  
  ## CALCULATE TEST STATISTIC
  
  theta_0 = null_hyp
  if (paired == "paired") {
    Z = data1 - data2
    z_abstract_str = paste("Z_i = (x_i-y_i) - ",theta_0,sep="")
  } else {
    Z = data1 - rep(theta_0,length(data1))
    z_abstract_str = paste("Z_i = x_i-",theta_0,sep="")
  }
  n = sum(Z!=0)
  psi_i = sign(Z)==1
  test_stat = sum(psi_i)
  
  psi_prototype_str = "\\[\\Psi_i=\\begin{cases}1, & Z_i > 0 \\\\ 0 , & Z_i < 0\\end{cases}\\]"
  
  test_stat_str = paste("B_c=\\Sigma\\Psi_i=",test_stat,sep="")
  
  calc_test_stat_str = paste("$$",z_abstract_str,"$$\n",psi_prototype_str,"\n$$",test_stat_str,"$$",sep="")
  
  ## CALCULATE P-VALUE
  p = 0.5
  distrib = paste("Binomial(",n,",",p,")",sep="")
  
  if (alt_hyp_type == "<") {
    p_val = pbinom(test_stat,n,p)
    p_call_str = paste("P(B\\leq B_c = \\verb|pbinom(B_c,n,p) = pbinom(",test_stat,",",n,",",p,")|",sep="")
    alter_param = "less"
  } else if (alt_hyp_type == ">") {
    p_val = 1-pbinom(test_stat-1,n,p)
    p_call_str = paste("P(B\\geq B_c = \\verb|1-pbinom(B_c-1,n,p) = pbinom(",test_stat-1,",",n,",",p,")|",sep="")
    alter_param = "greater"
  } else { # \neq
    if (test_stat < n/2) {
      p_val = 2*pbinom(test_stat,n,p)
      p_call_str = paste("2*P(B\\leq B_c) = \\verb|2*pbinom(B_c,n,p) = 2*pbinom(",test_stat,",",n,",",p,")|",sep="")
    } else {
      p_val = 2*(1-pbinom(test_stat-1,n,p))
      p_call_str = paste("2*P(B\\geq B_c) =\\verb| 2*(1-pbinom(B_c-1,n,p)) = 2*(1-pbinom(",test_stat-1,",",n,",",p,"))|",sep="")
    }
    alter_param = "two.sided"
  }
}


calc_p_val_str = paste("$$\\text{p-value} =",p_call_str," = ",round(p_val,digits=4),"$$",sep="")

## INTERPRET P-VALUE

p_val_percent_str = paste(round(p_val*100,digits=2),"\\%", sep="")
interpret_str = paste("If there is",null_hyp_expl_str,"we will see our data or more extreme",p_val_percent_str,"of the time.")

## STATE CONCLUSION

if (p_val < 0.001) {
  evidence_meas_str = "very strong evidence"
} else if (p_val < 0.01) {
  evidence_meas_str = "strong evidence"
} else if (p_val < 0.05) {
  evidence_meas_str = "fairly strong evidence"
} else if (p_val < 0.1) {
  evidence_meas_str = "some evidence"
} else {
  evidence_meas_str = "little to no evidence"
}
p_val_str = paste("(p = ",round(p_val,digits=4),")",sep="")
conc_str = paste("There is ",evidence_meas_str," in favor of ",alt_hyp_expl_str," ",p_val_str,".",sep="")

## RUN IN R
if (as.character(test_type) == "1") {
  r_t_test_command_str = paste("\\texttt{> t.test(diff,mu=",mu_0,",","alternative=\"",alter_param,"\")}",sep="")
  capt_out = capture.output(t.test(data1,mu=mu_0,alternative=alter_param))
  r_output_str = paste("\\texttt{",capt_out,"}\\\\",sep="")
} else if (as.character(test_type) == "2") {
  library(BSDA)
  if (paired == "paired") {
    r_t_test_command_str = paste("\\texttt{> SIGN.test(x=data1,y=data2,alternative=\"",alter_param,"\")}",sep="")
    capt_out = capture.output(SIGN.test(x=data1,y=data2,alternative=alter_param))
  } else {
    r_t_test_command_str = paste("\\texttt{> SIGN.test(x=data1,md=",theta_0,",alternative=\"",alter_param,"\")}",sep="")
    capt_out = capture.output(SIGN.test(x=data1,md=theta_0,alternative=alter_param))
  }
  r_output_str = paste("\\texttt{",capt_out,"}\\\\",sep="")
}

### OUTPUT

p_state_hyp <- "State the hypotheses for the sign test."
p_calc_test_stat <- "Calculate the test statistic (by hand)."
p_calc_p_val <- "Calculate the p-value (\"by hand\")."
p_interpret_p_val <- "Interpret the p-value."
p_conclusion <- "State your conclusion in terms of the problem."
p_run_in_R <- "Run the sign test in \\verb|R|. Display your command and output."


beg_enum <- "\\begin{enumerate}[label=\\fbox{\\alph*}]"
end_enum <- "\\end{enumerate}"
item <- "\\item" 

p_misc_1 <- "If the null is true, what is the distribution of the test statistic?"



cat(beg_enum,"\n")

cat(item,p_state_hyp,"\n")
cat(null_hyp_str,"\n")
cat(alt_hyp_str,"\n")

cat(item,p_calc_test_stat,"\n")
cat(calc_test_stat_str,"\n")

cat(item,p_misc_1,"\n")
cat("\\par The distribution is",distrib,"\n")

cat(item,p_calc_p_val,"\n")
cat(calc_p_val_str,"\n")

cat(item,p_interpret_p_val,"\n")
cat("\\par",interpret_str,"\n")

cat(item,p_conclusion,"\n")
cat("\\par",conc_str,"\n")

cat(item,p_run_in_R,"\n")
cat("\\par",r_t_test_command_str,"\n")
cat("\\par",r_output_str,"\n")


cat(end_enum,"\n")
