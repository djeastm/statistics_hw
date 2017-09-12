options(xtable.comment = FALSE)
## ENTER DATA
# lines = readline(prompt="How many lines of data? ")
# data_str = readline(prompt="Please enter data as a vector with spaces separating each entry: ")
# data1 = as.numeric(unlist(strsplit(data_str," ")))
# 
# if (as.character(lines) == "2") {
#   data_str = readline(prompt="Please enter data as a vector with spaces separating each entry: ")
#   data2 = as.numeric(unlist(strsplit(data_str," ")))
#   paired = "paired"
# } else {
#   paired = "not paired"
# }

## FOR TESTING ONLY: TAKE OUT!!

# t-Test
#data1 = c(91.6,95.9,114.5,93.6,112.7,93.6,99.2,93.2,105.9,110.8,127.0,91.5,100.5,104.9,104.8,124.1,124.6,109.1,98.7,95.8)
#data2 = c(96.6,97.5,113.9,88.9,108.3,99.0,97.3,87.5,96.8,102.1,114.5,100.9,100.0,100.7,99.4,117.1,111.3,102.2,95.3,91.1)

# Sign Test
#data1 = c(19.7,14.7,19.2,15.1,10.4,12.1,13.1,12.1,19.6,33.3)
#data2 = c()
#paired = "paired"

# Wilcoxon Sign Rank Test
data1_label = "MD"
# data1 = c(12500,22300,14500,32300,20800,19200,15800,17500,23300,42100,16800,14500)
data2_label = ""
# data2 = c(11750,20900,14800,29900,21500,18400,14500,17900,21400,43200,15200,14200)
data1 = c(164353, 148135, 168235, 168932, 152793, 165827, 161855,
          118044, 172665, 162262, 150408, 159091, 141299, 179262)
data2 = c()
 paired = "not paired"
 data_digits = 0

# data1_label = "Hours"
# data1 = c(4, 7.5, 8, 7, 6, 5, 5.5, 6.5, 7 ,7.5, 4.5)
# data2_label = ""
# data2 = c()
# paired = "not paired"
# data_digits = 1

# Both
test_type = "3"
null_hyp = 140616
#"If there is _______, we will see our data or more extreme...:"
null_hyp_expl_str = "a median debt amount for graduating MDs of \\$140,616"
alt_hyp_type = "\\neq"
#"There is [evidence measure] that ___________ (p = X.XXXX): "
# -ing tense
alt_hyp_expl_str = "doctors graduating with more than \\$140,616 in debt"

## FOR TESTING ONLY: TAKE OUT!!

# TYPE OF TES
##cat("Test types:\n")
##cat("1) One Sample t-Test\n")
##cat("2) Sign Test\n")
##cat("3) Wilcoxon Sign Rank Test\n")
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
} else if (as.character(test_type) == "3") {
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
    p_call_str = paste("\\texttt{pt(T,df) = pt(",test_stat,",",deg_free,")}",sep="")
    alter_param = "less"
  } else if (alt_hyp_type == ">") {
    p_val = 1-pt(test_stat,deg_free)
    p_call_str = paste("\\texttt{1-pt(T,df) = 1-pt(",test_stat,",",deg_free,")}",sep="")
    alter_param = "greater"
  } else { # \neq
    p_val = 2*(1-pt(abs(test_stat),deg_free))
    p_call_str = paste("\\texttt{2*pt(abs(T),df)) = 2*pt(abs(",test_stat,"),",deg_free,")|",sep="")
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
    p_call_str = paste("P(B\\leq B_c) = \\texttt{pbinom(B\\_c,n,p) = pbinom(",test_stat,",",n,",",p,")}",sep="")
    alter_param = "less"
  } else if (alt_hyp_type == ">") {
    p_val = 1-pbinom(test_stat-1,n,p)
    p_call_str = paste("P(B\\geq B_c) = \\texttt{1-pbinom(B\\_c-1,n,p) = pbinom(",test_stat-1,",",n,",",p,")}",sep="")
    alter_param = "greater"
  } else { # \neq
    if (test_stat < n/2) {
      p_val = 2*pbinom(test_stat,n,p)
      p_call_str = paste("2*P(B\\leq B_c) = \\texttt{2*pbinom(B\\_c,n,p) = 2*pbinom(",test_stat,",",n,",",p,")}",sep="")
    } else {
      p_val = 2*(1-pbinom(test_stat-1,n,p))
      p_call_str = paste("2*P(B\\geq B_c) =\\texttt{2*(1-pbinom(B\\_c-1,n,p)) = 2*(1-pbinom(",test_stat-1,",",n,",",p,"))}",sep="")
    }
    alter_param = "two.sided"
  }
} else if (as.character(test_type) == "3") {# in Wilcoxon Sign Rank Test Mode
  
  ## CALCULATE TEST STATISTIC
  big.frame = data.frame(1:length(data1))
  theta_0 = null_hyp
  if (paired == "paired") {
    Z = data1 - data2
    z_abstract_str = paste("Z_i = (x_i-y_i) - ",theta_0,sep="")
    big.frame[data1_label] = data1
    big.frame[data2_label] = data2
  } else {
    Z = data1 - rep(theta_0,length(data1))
    z_abstract_str = paste("Z_i = x_i-",theta_0,sep="")
    big.frame[data1_label] = data1
  }
  big.frame["Z"] = Z
  big.frame["|Z|"] = abs(Z)
  big.frame["Ranks"] = rep(-1,length(Z))
  big.frame["Psi"] = sign(Z)==1
  n = sum(big.frame["|Z|"]!=0)
  sorted_big.frame = big.frame[order(abs(Z)),]
  
  # n = sum(Z!=0)
  # no_zeros_Z =Z[Z!=0]
  # abs_Z = abs(no_zeros_Z)
  # rank_Z = rep(-1,n)
  # psi_Z = sign(no_zeros_Z)==1
  #   z.frame=data.frame(1:length(Z),n,no_zeros_Z, abs_Z,rank_Z, psi_Z)
  # sorted_z.frame = z.frame[order(abs_Z),]
  
  
  last = -1
  is_tied = F
  tie_start = -1
  ties_exist = F
  zero_count = 0
  for(i in 1:nrow(sorted_big.frame)) {
    if (sorted_big.frame[i,"Z"] == 0) {
      zero_count = zero_count + 1
    }
    else if (is_tied == F && sorted_big.frame[i,"|Z|"] == last) {
      is_tied = T
      ties_exist = T
      if (i == nrow(sorted_big.frame)) {
        tie_start = i-1
        avg = ((i) + tie_start) / 2
        for (k in tie_start:i) {
          sorted_big.frame[k,"Ranks"] = avg
        }
      } else {
        tie_start = i
      }
    } else if (sorted_big.frame[i,"|Z|"] == last) {
    } else if (is_tied == T) {
      # set all previous ones to tied average
      avg = ((i-1) + tie_start-1) / 2
      for (k in tie_start:i-1) {
        sorted_big.frame[k,"Ranks"] = avg
      }
      sorted_big.frame[i,"Ranks"] = i 
      last = sorted_big.frame[i,"|Z|"]
      # Not tied anymore
      is_tied = F
    } else {
      sorted_big.frame[i,"Ranks"] = i 
      last = sorted_big.frame[i,"|Z|"]
    }
  }
  
  sorted_big.frame["Ranks"] = sorted_big.frame["Ranks"]-zero_count 
  for(i in 1:nrow(sorted_big.frame)) {
    if (sorted_big.frame[i,"Z"] == 0) {
      sorted_big.frame[i,"Ranks"] = 0
    }
  }
  
  
  psi_prototype_str = "\\[\\Psi_i=\\begin{cases}1, & Z_i > 0 \\\\ 0 , & Z_i < 0\\end{cases}\\]"
  
  test_stat = sum(sorted_big.frame[which(sorted_big.frame[,"Psi"]== T),"Ranks"])
  test_stat_str = paste("T_{c}^{+}=\\Sigma\\Psi_i=",test_stat,sep="")
  
  calc_test_stat_str = paste("$$",z_abstract_str,"$$\n",psi_prototype_str,"\n$$",test_stat_str,"$$",sep="")
  
  sorted_big.frame = sorted_big.frame[order(sorted_big.frame[,1]),]
  display.frame = t(sorted_big.frame)
  display.frame = display.frame[-1,]
  
  if (paired == "paired") {
    digits_mat = matrix(c(data_digits,data_digits,data_digits,data_digits,1,0),nrow = length(display.frame[,1]), ncol=length(display.frame[1,])+1, byrow=FALSE)
  }
  else {
    digits_mat = matrix(c(data_digits,data_digits,data_digits,1,0),nrow = length(display.frame[,1]), ncol=length(display.frame[1,])+1, byrow=FALSE)
  }
  display_str = capture.output(xtable(display.frame,digits=digits_mat))
  
  ## CALCULATE P-VALUE
  if (alt_hyp_type == "<") {
    if (ties_exist == T) {
      p_call_str = paste("P(T^{+}\\leq T\\_{c}^{+}) &= \\texttt{psignrank(T\\_c\\_plus - 1,n)}\\\\ &= \\texttt{psignrank(",test_stat," - 1,",n,")}\\\\",sep="")
      test_stat = test_stat - 1
    } else {
      p_call_str = paste("P(T^{+}\\leq T\\_{c}^{+}) &= \\texttt{psignrank(T\\_c\\_plus,n)}\\\\ &= \\texttt{psignrank(",test_stat,",",n,")}\\\\",sep="")
      
    }
    p_val = psignrank(test_stat,n)
    alter_param = "less"
  } else if (alt_hyp_type == ">") {
    p_val = psignrank(test_stat-1,n,lower.tail=F)
    p_call_str = paste("P(T^{+}\\geq T_{c}^{+}) &= \\texttt{psignrank(T\\_c\\_plus - 1,n,lower.tail=F)}\\\\ &= \\texttt{psignrank(",test_stat," - 1,",n,",lower.tail=F)}\\\\",sep="")
    alter_param = "greater"
  } else { # \neq
    if (test_stat < (n*(n+1))/4) {
      if (ties_exist == T) {
        test_stat = test_stat - 1
      }
      p_val = 2*psignrank(test_stat,n)
      p_call_str = paste("2*P(T^{+}\\leq T_{c}^{+}) &= \\texttt{2*psignrank(T\\_c\\_plus,n)}\\\\ &= \\texttt{2*psignrank(",test_stat,",",n,")}\\\\",sep="")
    } else if ((test_stat > (n*(n+1))/4)) {
      p_val = 2*psignrank(test_stat-1,n,lower.tail=F)
      p_call_str = paste("2*P(T^{+}\\geq T_{c}^{+}) &= \\texttt{2*psignrank(T\\_c\\_plus-1,n)}\\\\ &= \\texttt{2*psignrank(",test_stat," - 1,",n,")}\\\\",sep="")
    }
    alter_param = "two.sided"
  }
}


calc_p_val_str = paste("\\begin{align*}\\text{p-value} =",p_call_str," &= ",round(p_val,digits=4),"\\end{align*}",sep="")

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
} else if (as.character(test_type) == "3") {
  library(exactRankTests)
  if (paired == "paired") {
    r_t_test_command_str = paste("\\texttt{> wilcox.exact(data1,data2,alternative=\"",alter_param,"\",paired=T)}",sep="")
    capt_out = capture.output(wilcox.exact(data1,data2,alternative=alter_param,paired=T))
  } else {
    r_t_test_command_str = paste("\\texttt{> wilcox.exact(data1,mu=",theta_0,",","alternative=\"",alter_param,"\")}",sep="")
    capt_out = capture.output(wilcox.exact(data1,mu=theta_0,alternative=alter_param))
  }
  r_output_str = paste("\\texttt{",capt_out,"}\\\\",sep="")
}

### OUTPUT

p_state_hyp <- "State the hypotheses of the test."
p_calc_test_stat <- "Calculate the test statistic by hand."
p_calc_p_val <- "Calculate the p-value (``by hand\")."
p_interpret_p_val <- "Interpret the p-value in context."
p_conclusion <- "State the conclusion of the test in context."
p_run_in_R <- "Run the Signed Rank test in \\verb|R|. Report your function and output."


beg_enum <- "\\begin{enumerate}[label=\\fbox{\\alph*}]"
end_enum <- "\\end{enumerate}"
item <- "\\item" 

p_misc_1 <- "What makes this paired data rather than two independent groups? Why do we
want to use pairing??"



cat(beg_enum,"\n")

#cat(item,p_misc_1,"\n")

cat(item,p_state_hyp,"\n")
cat(null_hyp_str,"\n")
cat(alt_hyp_str,"\n")

cat(item,p_calc_test_stat,"\n")
cat(display_str,"\n")
cat(calc_test_stat_str,"\n")

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
