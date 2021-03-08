## Exercise 4
austen=read.table("austen.txt",header=TRUE)
austendata=data.frame(austen$word, austen$Sense, austen$Emma, austen$Sand1)

attach(austendata)

# a) For a statistical analysis of the word frequencies in relation to Jane Austen's novel Sanditon,
# the amount of specific words used in her previous works is compared to the ones used by the admirer who tried to replicate her style.
# For such comparison, it is needed to determine if the frequency counts for specific words are equally distributed for both writers. 
# Therefore, a contingency table test for homogenity is the most suitable.

# b) Austen's consistency in different novels

# To analyse Austen's consistency throughout her different novels, a Chi-squared test can be used.
# This is performed on the columns containing data based on her writings (Sense, Emma, Sand1): 
consistency=chisq.test(austendata)

# Pearson's Chi-squared test: X-squared = 12.271, df = 10, p-value = 0.2673

#Since the p-value is high (0.26), the null hypothesis is to be rejected, therefore confirming a relationship between the variables.
# As expected, it can be concluded that there is a consistency throughout Austen's novels.

# The main inconsistencies can be found by investigating the residuals:
residuals(consistency)  # SHOULD add the 1st column here (word to count) ??


# c) To check whether the admirer was successful in imitating Austen's style, a Chi-squared test including all data was performed:
admirer=chisq.test(austen)

#Pearson's Chi-squared test: X-squared = 45.578, df = 15, p-value = 6.205e-05

#Since the p-value is very high (6.205e-05), it can be concluded that the null hypothesis is rejected, thus confirming a similarity between the writers' style.

# The main differences between the writers can be found once again by inspecting the residuals:

residuals(chisq.test(austen))

# The main differences are found for the count of the following words:
# - "an" and "with" - both words have been used significantly more by the admirer, compared to Austen
# - "that" - has been used significantly less by the admirer, compared to Austen 

# Therefore, it can be concluded that the admirer was quite sucessfull in imitating Austen's writing style, with small differences for a few words.
