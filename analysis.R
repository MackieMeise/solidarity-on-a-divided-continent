#### DATA EXPLORATION ####

m <- df1 %>%
  group_by(id) %>%
  summarise(count=n())

summary(m$count)

summary(df1$solidaritysalience)
m <- filter(df1, solidaritysalience==1)
summary(m$solidarityfeel)


# plotting solidarity salience and feeling on aggregated level
df_test <- df1 %>%
  group_by(country, reccountry, rectaxonomy2) %>%
  summarise(count=n(), solsal=sum(solidaritysalience), solfee=sum(solidarityfeel)) %>%
  mutate(sallvl=solsal/count, feelvl=solfee/count, solnet=solfee/solsal)

# analyse response patterns of solidarity variable

# the graph shows the profiles of respondents - some show solidarity regardless of the country, other show none regardless. But many do indeed distinguish between countries
p <- df1 %>%
  group_by(id, country) %>%
  summarise(sol=sum(solidarityfeel), dksol=sum(solidaritysalience), count=n()) %>%
  mutate(profile=ifelse(dksol == count & sol == count,"fullsolidarity",ifelse(dksol==0,"noopinion",ifelse(sol>=0.5*dksol,"mostly solidary","mostly not solidary"))))

ggplot(p, aes(x=country, fill=profile)) +
  geom_bar(position="fill")

#### MULTILEVEL LOGISTIC REGRESSION ####

# clean dataset aas needed
df2 <- df1 %>%
  mutate(demosat=as.character(demosat),
         polselfpl=as.character(polselfpl),
         inflcntry=as.character(inflcntry),
         inflpers=as.character(inflpers),
         income=as.character(income),
         fundben=as.character(fundben)) %>%
  mutate(demosat=ifelse(is.na(demosat),"NA", demosat),
         polselfpl=ifelse(is.na(polselfpl),"NA",polselfpl),
         inflcntry=ifelse(is.na(inflcntry),"NA",inflcntry),
         inflpers=ifelse(is.na(inflpers),"NA",inflpers),
         income=ifelse(is.na(income),"NA",income),
         fundben=ifelse(is.na(fundben),"NA",fundben)) %>%
  mutate(demosat=as.factor(demosat),
         polselfpl=as.factor(polselfpl),
         inflcntry=as.factor(inflcntry),
         inflpers=as.factor(inflpers),
         income=as.factor(income),
         fundben=as.factor(fundben))

df2 <- na.omit(df2)
### HAND-MADE SAMPLE SELECTION MODEL ====

#### Run Everything as a Model with Cluster-Robust Standard Errors ----
cluster_selectionmodel <- function(selformula, outformula, cluster, data) {
  m1_s <- glm.cluster(selformula,
                      data=data,
                      cluster=cluster,
                      family=binomial(link="probit"))
  
  first_stage_lp <- predict(m1_s$glm_res)
  data$IMR <- dnorm(first_stage_lp)/pnorm(first_stage_lp)
  
  m1_o <- glm.cluster(outformula,
                      data=data,
                      cluster=cluster,
                      family=binomial(link="probit"))
  return(list(selm=m1_s, outm=m1_o))
  
}
m1 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + IMR,
                             data=df2,
                             cluster=df2$id)

m2 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + IMR,
                             data=df2,
                             cluster=df2$id)

m3 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*inflcntry,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*inflcntry + IMR,
                             data=df2,
                             cluster=df2$id)

m4 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry + IMR,
                             data=df2,
                             cluster=df2$id)
screenreg(list(m1$outm, m2$outm, m3$outm, m4$outm))
htmlreg(list(m1$outm, m2$outm, m3$outm, m4$outm), file="regtable.doc", single.row=T, stars=0.01)
screenreg(list(m1$selm, m2$selm, m3$selm, m4$selm))
htmlreg(list(m1$selm, m2$selm, m3$selm, m4$selm), file="regtable_selm.doc", single.row=T, stars=0.01)
summary(m1$selm)

p1 <- plot_model(m4$outm$glm_res, type="pred", terms=c("taxonomy2", "rectaxonomy2"), ci.lvl=0.99) +
  labs(title="", y="Predicted probability of solidarity", x="Cleavage group of respondent") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Cleavage group of recipient country") +
  scale_fill_manual(values=c("gray", "lightgray"), name="Cleavage group of recipient country") +
  scale_y_continuous(limits=c(0.3,0.6))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))

p2 <- plot_model(m4$outm$glm_res, type="pred", terms=c("taxonomy2", "inflcntry[Agree, Disagree]"), ci.lvl=0.99) +
  labs(title="", y="", x="Cleavage group of respondent") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Sociotropic political efficacy") +
  scale_fill_manual(values=c("gray", "lightgray"), name="Sociotropic political efficacy") +
  scale_y_continuous(limits=c(0.3,0.6))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))

ggarrange(p1,p2, ncol=2, labels="AUTO", common.legend=F)
ggsave("interaction_bw.png", width=190, height=130, unit="mm")

# LRT for the interaction effects
anova(m4$outm$glm_res, m3$outm$glm_res, test="LRT")
anova(m4$outm$glm_res, m2$outm$glm_res, test="LRT")
anova(m3$outm$glm_res, m1$outm$glm_res, test="LRT")
anova(m2$outm$glm_res, m1$outm$glm_res, test="LRT")

#R2
with(summary(m1$outm$glm_res), 1 - deviance/null.deviance)
with(summary(m2$outm$glm_res), 1 - deviance/null.deviance)
with(summary(m3$outm$glm_res), 1 - deviance/null.deviance)
with(summary(m4$outm$glm_res), 1 - deviance/null.deviance)

#### Run Everything as a Multi-level Model ----

multilevel_selectionmodel <- function(selformula, outformula, data) {
  m1_s <- glmer(selformula,
                data=data,
                family=binomial(link="probit"),
                nAGQ = 0)
  
  first_stage_lp <- predict(m1_s)
  data$IMR <- dnorm(first_stage_lp)/pnorm(first_stage_lp)
  
  m1_o <- glmer(outformula,
                data=data,
                family=binomial(link="probit"),
                nAGQ = 0)
  return(list(selm=m1_s, outm=m1_o))
  
}

m1ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + year + (1|id),
                                  outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + IMR + year + (1|id),
                                  data=df2)

m2ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2+ year + (1|id),
                                  outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + IMR+ year + (1|id),
                                  data=df2)

m3ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*inflcntry+ year + (1|id),
                                  outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*inflcntry + IMR+ year + (1|id),
                                  data=df2)

m4ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry+ year + (1|id),
                                  outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry + IMR+ year + (1|id),
                                  data=df2)
screenreg(list(m1ml$outm, m2ml$outm, m3ml$outm, m4ml$outm))

export_summs(m1ml$outm,m2ml$outm,m3ml$outm,m4ml$outm)

export_summs(m1ml$outm, r.squared=F)
m1_ma <- margins(m1ml$outm)
export_summs(m1_ma, m2_ma, m3_ma, m4_ma)
export_summs(m1ml$outm, m1_ma, r.squared=F)
screenreg(list(mod1, mod2))

#### experimental play around ####

p <- df1 %>%
  group_by(country, reccountry) %>%
  summarise(solidarity=sum(ifelse(solidarity=="1",1,0)/n()))

p2 <- p %>%
  group_by(reccountry) %>%
  summarise(solidarity=mean(solidarity))
p3 <- p %>%
  group_by(country) %>%
  summarise(solidarity=mean(solidarity))
px <- left_join(p2,p3, by=c("reccountry" = "country"))
ggplot(px, aes(solidarity.x, solidarity.y)) + geom_point() + geom_label(aes(label=reccountry)) + geom_abline(slope=1,intercept=0)

m2 <- glmer(solidarity ~ 1 + (1|id) + (1|country), data=filter(df1,solidarity!=99 & country != "United Kingdom"), family="binomial",nAGQ=0)
m3 <- glmer(solidarity ~ border+ eurombmr*receurombmr + (1|id) + (1|country), data=filter(df1, solidarity!=99 & country != "United Kingdom"), family="binomial", nAGQ=0)
m4 <- glmer(solidarity ~ eurombmr*receurombmr + weumbrshp*rectweumbrshp + (1|id) + (1|country), data=filter(df1, solidarity!=99 & country != "United Kingdom"), family="binomial", nAGQ=0)
m5 <- glmer(solidarity ~ border + moneyspent2 + eurombmr*receurombmr + weumbrshp*rectweumbrshp + (1|id) + (1|country), data=filter(df1, solidarity!=99 & country != "United Kingdom"), family="binomial", nAGQ=0)
screenreg(list(m5))
plot_model(m5, type="pred", terms=c("eurombmr","receurombmr"))
plot_model(m5, type="pred", terms=c("border"))

mx4 <- glmer(solidarity ~ inflcntry*rectaxonomy + (1|id) + (1|country), data=df1, family="binomial", nAGQ=0)

screenreg(mx4)
screenreg(list(mx1,mx2,mx3))
plot_model(mx3, type="pred", terms=c("taxonomy", "rectaxonomy"))
plot_model(mx3, type="pred", terms=c("eurombmr", "receurombmr"))
plot_model(mx4, type="pred", terms=c("inflcntry", "rectaxonomy"))

vif(mx2)
mx4 <- glmer(solidarity ~ moneyspent2 + eurombmr*receurombmr + border + culdis + (1|id) + (1|country), data=filter(df1, solidarity!=99 & eurombmr != "0" & receurombmr != "0"), family="binomial", nAGQ=0)
plot_model(mx4, type="pred", terms=c("eurombmr", "receurombmr"))

newdf <- filter(df1,solidarity!=99 & country != "United Kingdom") %>%
  mutate(yhat=predict(mx, type="response", newdata=filter(df1,solidarity!=99 & country != "United Kingdom")))



rawdata <- rawdata %>%
  mutate(timeA=strptime(endtime,format="%d/%m/%Y %H:%M"), timeB=strptime(starttime,format="%d/%m/%Y %H:%M")) %>%
  mutate(time=timeA-timeB)

ggplot(rawdata, aes(x=time, y=age_grp_all)) +
  geom_point() +
  geom_smooth(method="lm")
plot(rawdata$time~rawdata$q12a_1)