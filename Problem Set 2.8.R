# Here are three students with very different approaches to constructing priors.
probability <- data.frame(NumberOfSixes=c(1:5),
                          likelihood=numeric(5))
for (i in 1:5) {
  probability[i,2]=(choose(20,7))*((i/6)^7)*(((6-i)/6)^13)
}
# Aditi: My experience in the world suggests that most playing dice have exactly one 6.
# In fact, probably all dice I have ever seen had exactly one 6. So P(H1) would be close to 1.
# So, I set P(H1) to 0.99 and P(H2) = P(H3) = P(H4) = P(H5) = 0.0025.
probability$prior_case1 <- c(0.99, replicate(4, 0.0025))
probability$posterior_case1 <- probability$prior_case1[1]/probability$prior_case1*probability$likelihood

# Bo: We know nothing about this die, and dealing with different priors is complicated, 
# so we may as well assume P(Hi) = 0.2 for all i (1 < i < 5).
probability$prior_case2 <- c(replicate(5, 0.2))
probability$posterior_case2 <- probability$prior_case2[1]/probability$prior_case2*probability$likelihood

# Charlie: I know my friend to be a trickster and I don’t trust them. 
# I am 99 % sure the die is not fair, therefore P(H1) = 0.01. 
# I don’t know how unfair exactly, 
# so I assume P(H2) = P(H3) = P(H4) = P(H5) = 0.2475.
probability$prior_case3 <- c(0.01, replicate(4, 0.2475))
probability$posterior_case3 <- probability$prior_case3[1]/probability$prior_case3*probability$likelihood


# Does the choice of prior influence the end result?
# A: yes.
# Also, why do the priors for each person have to add up to 1?

# And why does nobody choose priors of exactly 0 or exactly 1?

# What would happen if they did?





