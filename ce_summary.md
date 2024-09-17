Consumer WTP for Carbon Offsets
================

# Basic Logit Model

    ## 
    ## Model estimated on: Tue Sep 17 04:44:05 PM 2024 
    ## 
    ## Call:
    ## gmnl(formula = f, data = dt, model = "mnl", method = "nr")
    ## 
    ## Frequencies of categories:
    ## 
    ##        1        2        3        4        5        6 
    ## 0.192868 0.272962 0.158307 0.243260 0.060815 0.071787 
    ## 
    ## The estimation took: 0h:0m:9s 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error  z-value  Pr(>|z|)    
    ## I                 -0.9858758  0.0355302 -27.7476 < 2.2e-16 ***
    ## price             -0.0318596  0.0018653 -17.0803 < 2.2e-16 ***
    ## location_EU       -0.0193157  0.0179737  -1.0747    0.2825    
    ## location_UK        0.1726739  0.0178102   9.6952 < 2.2e-16 ***
    ## certificate_NGO    0.0993108  0.0182718   5.4352 5.474e-08 ***
    ## certificate_UK     0.3372772  0.0185896  18.1433 < 2.2e-16 ***
    ## project_renewable  0.1324178  0.0209696   6.3148 2.706e-10 ***
    ## project_landfill  -0.2605849  0.0230259 -11.3170 < 2.2e-16 ***
    ## project_manure    -0.1361997  0.0209358  -6.5056 7.740e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Optimization of log-likelihood by Newton-Raphson maximisation
    ## Log Likelihood: -20883
    ## Number of observations: 12760
    ## Number of iterations: 4
    ## Exit of MLE: gradient close to zero (gradtol)

# Mixed Logit Model

    ## 
    ## Model estimated on: Tue Sep 17 04:44:05 PM 2024 
    ## 
    ## Call:
    ## gmnl(formula = f, data = dt, model = "mixl", ranp = randpar, 
    ##     R = 2000, haltons = NA, panel = T, method = "bhhh", iterlim = 5000)
    ## 
    ## Frequencies of categories:
    ## 
    ##        1        2        3        4        5        6 
    ## 0.192868 0.272962 0.158307 0.243260 0.060815 0.071787 
    ## 
    ## The estimation took: 0h:19m:58s 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error  z-value Pr(>|z|)    
    ## I                    -1.7076129  0.0524453 -32.5599   <2e-16 ***
    ## price                -0.0535081  0.0028010 -19.1030   <2e-16 ***
    ## location_EU          -0.0067488  0.0258659  -0.2609   0.7942    
    ## location_UK           0.4179854  0.0264374  15.8104   <2e-16 ***
    ## certificate_NGO       0.2307550  0.0259901   8.8786   <2e-16 ***
    ## certificate_UK        0.7014281  0.0299040  23.4560   <2e-16 ***
    ## project_renewable     0.3764557  0.0310886  12.1091   <2e-16 ***
    ## project_landfill     -0.4942491  0.0352026 -14.0401   <2e-16 ***
    ## project_manure       -0.2840401  0.0303703  -9.3526   <2e-16 ***
    ## sd.price              0.1326451  0.0030584  43.3701   <2e-16 ***
    ## sd.location_EU        0.8132208  0.0382788  21.2447   <2e-16 ***
    ## sd.location_UK        0.9694256  0.0374176  25.9083   <2e-16 ***
    ## sd.certificate_NGO    0.6449726  0.0426455  15.1240   <2e-16 ***
    ## sd.certificate_UK     0.7242212  0.0381519  18.9826   <2e-16 ***
    ## sd.project_renewable  1.0763405  0.0420458  25.5992   <2e-16 ***
    ## sd.project_landfill   0.7209225  0.0485994  14.8340   <2e-16 ***
    ## sd.project_manure     0.8636869  0.0419620  20.5826   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Optimization of log-likelihood by BHHH maximisation
    ## Log Likelihood: -17176
    ## Number of observations: 12760
    ## Number of iterations: 34
    ## Exit of MLE: successive function values within relative tolerance limit (reltol)
    ## Simulation based on 2000 draws

# mixed logit + co2 consumption + framing effect

    ## 
    ## Model estimated on: Tue Sep 17 04:44:05 PM 2024 
    ## 
    ## Call:
    ## gmnl(formula = f, data = dt, model = "mixl", ranp = randpar, 
    ##     R = 2000, haltons = NA, mvar = mvarlist, panel = T, method = "bhhh", 
    ##     iterlim = 5000)
    ## 
    ## Frequencies of categories:
    ## 
    ##        1        2        3        4        5        6 
    ## 0.192868 0.272962 0.158307 0.243260 0.060815 0.071787 
    ## 
    ## The estimation took: 0h:23m:47s 
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error  z-value  Pr(>|z|)    
    ## I                                           -1.7087811  0.0525524 -32.5157 < 2.2e-16 ***
    ## price                                       -0.0529973  0.0028051 -18.8935 < 2.2e-16 ***
    ## location_EU                                  0.2297873  0.0631561   3.6384 0.0002743 ***
    ## location_UK                                  0.6592366  0.0649136  10.1556 < 2.2e-16 ***
    ## certificate_NGO                              0.3761863  0.0633979   5.9337 2.961e-09 ***
    ## certificate_UK                               0.8214735  0.0680741  12.0673 < 2.2e-16 ***
    ## project_renewable                            0.5329815  0.0710548   7.5010 6.328e-14 ***
    ## project_landfill                            -0.5588110  0.0848326  -6.5872 4.481e-11 ***
    ## project_manure                              -0.4307069  0.0733652  -5.8707 4.339e-09 ***
    ## location_EU.co2_value                       -0.0515600  0.0193603  -2.6632 0.0077406 ** 
    ## location_EU.framing_effectconsequence       -0.1427284  0.0766982  -1.8609 0.0627569 .  
    ## location_EU.framing_effectMetOffice         -0.2488516  0.0654199  -3.8039 0.0001424 ***
    ## location_EU.framing_effectUN                -0.1229429  0.0759659  -1.6184 0.1055772    
    ## location_UK.co2_value                       -0.0981726  0.0195277  -5.0273 4.973e-07 ***
    ## location_UK.framing_effectconsequence       -0.1431396  0.0797776  -1.7942 0.0727759 .  
    ## location_UK.framing_effectMetOffice         -0.1165991  0.0672205  -1.7346 0.0828159 .  
    ## location_UK.framing_effectUN                -0.0715614  0.0773786  -0.9248 0.3550589    
    ## certificate_NGO.co2_value                   -0.0855185  0.0197835  -4.3227 1.541e-05 ***
    ## certificate_NGO.framing_effectconsequence    0.0177280  0.0793446   0.2234 0.8232009    
    ## certificate_NGO.framing_effectMetOffice     -0.0382890  0.0667229  -0.5739 0.5660683    
    ## certificate_NGO.framing_effectUN             0.0159552  0.0777513   0.2052 0.8374092    
    ## certificate_UK.co2_value                    -0.0638677  0.0198679  -3.2146 0.0013062 ** 
    ## certificate_UK.framing_effectconsequence    -0.0204585  0.0792923  -0.2580 0.7963967    
    ## certificate_UK.framing_effectMetOffice      -0.0281686  0.0681578  -0.4133 0.6793980    
    ## certificate_UK.framing_effectUN              0.0093899  0.0794302   0.1182 0.9058967    
    ## project_renewable.co2_value                 -0.0584194  0.0217111  -2.6908 0.0071289 ** 
    ## project_renewable.framing_effectconsequence  0.0738383  0.0859580   0.8590 0.3903384    
    ## project_renewable.framing_effectMetOffice   -0.2388156  0.0717141  -3.3301 0.0008681 ***
    ## project_renewable.framing_effectUN           0.0883220  0.0835087   1.0576 0.2902203    
    ## project_landfill.co2_value                   0.0489535  0.0252393   1.9396 0.0524313 .  
    ## project_landfill.framing_effectconsequence   0.0947417  0.1023909   0.9253 0.3548130    
    ## project_landfill.framing_effectMetOffice    -0.1200169  0.0872628  -1.3754 0.1690228    
    ## project_landfill.framing_effectUN            0.0753729  0.0992251   0.7596 0.4474845    
    ## project_manure.co2_value                     0.0259304  0.0221016   1.1732 0.2407020    
    ## project_manure.framing_effectconsequence     0.1254465  0.0889345   1.4105 0.1583777    
    ## project_manure.framing_effectMetOffice       0.1168059  0.0761603   1.5337 0.1251071    
    ## project_manure.framing_effectUN              0.1464021  0.0887255   1.6501 0.0989314 .  
    ## sd.price                                     0.1325722  0.0030715  43.1624 < 2.2e-16 ***
    ## sd.location_EU                               0.8141358  0.0385990  21.0922 < 2.2e-16 ***
    ## sd.location_UK                               0.9621519  0.0376211  25.5748 < 2.2e-16 ***
    ## sd.certificate_NGO                           0.6435946  0.0428567  15.0174 < 2.2e-16 ***
    ## sd.certificate_UK                            0.7234437  0.0382575  18.9098 < 2.2e-16 ***
    ## sd.project_renewable                         1.0691508  0.0420963  25.3978 < 2.2e-16 ***
    ## sd.project_landfill                          0.7178810  0.0485417  14.7890 < 2.2e-16 ***
    ## sd.project_manure                            0.8651016  0.0423021  20.4506 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Optimization of log-likelihood by BHHH maximisation
    ## Log Likelihood: -17144
    ## Number of observations: 12760
    ## Number of iterations: 33
    ## Exit of MLE: successive function values within relative tolerance limit (reltol)
    ## Simulation based on 2000 draws

# mixed logit + co2 consumption + framing effect + PCA
