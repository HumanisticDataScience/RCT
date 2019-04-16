#Useful Code Snippets
#gives you this: [[ suppressing 23 column names ‘s0’, ‘s1’, ‘s2’ ... ]]
#s is lambda. giving you diff coef at diff lambdda values
cos <- as.matrix(coef(glm_mod$finalModel, glm_mod$finalModel$lambdaOpt))

#Get indices of vars with non-zero coefs
 inds <- which(cos != 0)
 
 #get names of non-zero coefs based on their indices above
 vars <- row.names(cos)[inds]

 #Which variables are included in final model? 
  which(vars %in% colnames(full_train))

#convert names of coefs and coef values to a dataframe for viz
  glm_coef <- data.frame(words = row.names(cos), co_ef= cos[,1])

  #Quick summary of coef values
  summary(glm_coef$co_ef)

#how many had positive impact (increase) on cosponsors?
length(subset(glm_coef$co_ef, glm_coef$co_ef > 0))


#how many had negative impact (fewer) on cosponsors?
length(subset(glm_coef$co_ef, glm_coef$co_ef < 0))


#plot dist of coefs
ggplot(glm_coef, aes(co_ef))+
  geom_density()+
  theme_minimal()+
  labs(title='Distribution of glmnet coefficients', x='estimated coefficient')

#plot top pos and top neg
pos <- head(glm_coef%>%filter(words != '(Intercept)')%>%arrange(desc(co_ef)),25)

neg <- tail(glm_coef%>%filter(words != '(Intercept)')%>%arrange(desc(co_ef)),25)
top50 <- rbind(pos, neg)

ggplot(top50, aes(co_ef, y=reorder(words, co_ef)))+
  geom_segment(aes(yend=words), xend=0)+
  geom_point(size=2, aes(color = co_ef))+
  scale_color_gradient2(name='Coefficients', low='red', midpoint = 1,mid='white', high='blue')+
  theme_minimal()+
  labs(title='Most important predictors and their impact on cosponsors',x='Betas', y='')


#this seems to work. coefficients at optimal lambda
co <- coef(glm_mod$finalModel, glm_mod$finalModel$lambda)

#which had positive impact
co[co > 1]
