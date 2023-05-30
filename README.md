# hybrid_environment
Explaining q score across >2 species by environmental variables

Currently, this is motivated by Amanda Meuser's and Samantha Share's projects looking at fish species across various environments and replicate hybrid zones. I will aslo extend this back to red deer and sika, recognizing that k=2 is a special case (and that with IBD, I might not have k=2).

Things to account for:

* Dirichlet (Proportional) distribution. Each animal has only one genome, which means that their q estimates for each species must add up to 1

* Even replicate hybrid zones with the exact same starting conditions are expected to vary (McFarlane et al. bioRxiv). For this reason, before looking at environmental effects, we need to account for the among population variation. 

* Because of these two things, we may need to implement in STAN, but I'm hoping that this can be done in DirichletReg. Not yet sure if it has the possibility of random effects or grouping variables

* Long term goal - package that includes the ability to call for environmental data

* Long term goal - simulations that demonstrate that we can actually do this

* short term goal - analyses that work for Amanda's and Sam's projects

* possible short cut - categories in brms rather than q scores. 