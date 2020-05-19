
<!-- README.md is generated from README.Rmd. Please edit that file -->

## PhenoAsync

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/kongdd/PhenoAsync.svg?branch=master)](https://travis-ci.org/kongdd/PhenoAsync)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/kongdd/PhenoAsync?branch=master&svg=true)](https://ci.appveyor.com/project/kongdd/PhenoAsync)
[![codecov](https://codecov.io/gh/kongdd/PhenoAsync/branch/master/graph/badge.svg)](https://codecov.io/gh/kongdd/PhenoAsync)
<!-- badges: end -->

Extract vegetation phenology by `phenofit` (v0.2.7) at 95 flux sites.

## Vegetation Phenology Data at 95 flux sites

There are two data files:

  - `pheno_flux95_prim ((GPP_DT) v0.2.6.9000).rda`: vegetation carbon
    phenology from GPP\_DT
  - `pheno_flux95_prim ((GPP_NT) v0.2.6.9000).rda`: vegetation carbon
    phenology from GPP\_NT

For each file, two variables are in there:

  - `df_gpp_prim`: vegetation carbon phenological metrics extracted from
    flux sites GPP observations  
  - `df_VI_prim` : vegetation greenness phenological metrics extracted
    from MODIS remote sensing vegetation indexes

<!-- end list -->

``` r
load("INPUT/pheno_flux95_prim ((GPP_NT) v0.2.6.9000).rda")
# load("INPUT/pheno_flux95_prim ((GPP_DT) v0.2.6.9000).rda")
print(df_gpp_prim)
#         site   meth   flag     origin TRS1.sos TRS1.eos TRS2.sos TRS2.eos
#    1: DE-Seh     AG 2008_1 2008-01-01       69      203       82      199
#    2: DE-Seh     AG 2009_1 2009-01-01       76      203       86      191
#    3: DE-Seh     AG 2010_1 2010-01-01      148      301      154      294
#    4: DE-Seh   Beck 2008_1 2008-01-01       70      203       83      199
#    5: DE-Seh   Beck 2009_1 2009-01-01       75      201       86      191
#   ---                                                                    
# 2538: CZ-BK2 Elmore 2012_1 2012-01-01      114      214      118      212
# 2539: CZ-BK2  Zhang 2009_1 2009-01-01      105      215      115      213
# 2540: CZ-BK2  Zhang 2010_1 2010-01-01      114      206      123      202
# 2541: CZ-BK2  Zhang 2011_1 2011-01-01      101      201      108      197
# 2542: CZ-BK2  Zhang 2012_1 2012-01-01      112      213      119      210
#       TRS5.sos TRS5.eos TRS6.sos TRS6.eos TRS8.sos TRS8.eos TRS9.sos TRS9.eos
#    1:      105      191      111      189      125      185      136      181
#    2:      103      174      108      169      118      159      125      153
#    3:      163      278      166      273      171      261      174      250
#    4:      105      191      110      189      124      185      135      181
#    5:      101      177      105      173      115      163      121      156
#   ---                                                                        
# 2538:      122      206      124      202      126      190      130      176
# 2539:      131      205      135      201      147      191      157      187
# 2540:      138      192      142      188      151      180      158      174
# 2541:      120      187      123      183      131      173      139      166
# 2542:      130      202      133      198      141      188      148      180
#       DER.sos DER.pop DER.eos  UD  SD  DD  RD Greenup Maturity Senescence
#    1:     105     169     191  72 136 180 204      58      157        174
#    2:     107     140     169  80 127 151 198      69      133        141
#    3:     166     182     281 151 176 257 302     144      181        246
#    4:     105     166     192  75 133 181 203      70      139        176
#    5:     102     139     175  80 121 157 197      75      127        150
#   ---                                                                    
# 2538:     122     146      NA 115 129 199  NA     109      135        154
# 2539:     131     184      NA 108 154 196  NA     103      159        188
# 2540:     139     163     197 118 157 178  NA     113      160        174
# 2541:     120     162     195 103 136 173  NA      99      142        165
# 2542:     130     172      NA 115 146 191  NA     110      150        181
#       Dormancy GSL
#    1:      209  86
#    2:      208  71
#    3:      312 115
#    4:      207  86
#    5:      202  76
#   ---             
# 2538:       NA  84
# 2539:       NA  74
# 2540:       NA  54
# 2541:       NA  67
# 2542:       NA  72
print(df_VI_prim)
#             sate type_VI   site group  meth   flag     origin TRS1.sos TRS1.eos
#      1: combined     EVI AT-Neu     1    AG 2003_1 2003-01-01       97      357
#      2: combined     EVI AT-Neu     1    AG 2004_1 2004-01-01      108      366
#      3: combined     EVI AT-Neu     1    AG 2005_1 2005-01-01       98      346
#      4: combined     EVI AT-Neu     1    AG 2006_1 2006-01-01      117      347
#      5: combined     EVI AT-Neu     1    AG 2007_1 2007-01-01      100      362
#     ---                                                                        
# 643422:    Terra NDVI_pc ZM-Mon     9 Zhang 2014_1 2014-01-01      267      530
# 643423:    Terra NDVI_pc ZM-Mon     9 Zhang 2015_1 2015-01-01      265      524
# 643424:    Terra NDVI_pc ZM-Mon     9 Zhang 2016_1 2016-01-01      270      533
# 643425:    Terra NDVI_pc ZM-Mon     9 Zhang 2017_1 2017-01-01      264      510
# 643426:    Terra NDVI_pc ZM-Mon     9 Zhang 2018_1 2018-01-01      317      527
#         TRS2.sos TRS2.eos TRS5.sos TRS5.eos TRS6.sos TRS6.eos TRS8.sos TRS8.eos
#      1:      107      331      125      285      130      273      143      247
#      2:      122      339      149      292      156      279      172      252
#      3:      108      326      126      290      130      279      140      253
#      4:      132      321      156      280      163      269      176      246
#      5:      104      336      115      288      118      274      124      242
#     ---                                                                        
# 643422:      279      511      299      479      305      470      318      447
# 643423:      280      506      305      476      312      467      332      446
# 643424:      287      513      316      481      325      471      345      449
# 643425:      277      493      301      465      309      457      327      439
# 643426:      326      508      341      476      346      466      356      443
#         TRS9.sos TRS9.eos DER.sos DER.pop DER.eos  UD  SD  DD  RD Greenup
#      1:      154      231     123     199     272  97 151 225 346      86
#      2:      181      234     156     200     279 113 185 231 355      98
#      3:      146      234     130     158     292 103 148 243 344      93
#      4:      185      231     163     202     268 124 188 228 334     111
#      5:      129      218     116     138     287  NA 130 228 358      NA
#     ---                                                                  
# 643422:      331      427     299     393     479 272 326 439 524     267
# 643423:      340      426     304     342     476 268 342 440 519     261
# 643424:      363      429     316     377     480 274 357 439 527     268
# 643425:      344      421     299     390     466 260 338 430 505     253
# 643426:      364      422     341     400     476 320 362 436 524     316
#         Maturity Senescence Dormancy GSL
#      1:      165        200      370 160
#      2:      199        216      384 143
#      3:      157        174      368 164
#      4:      201        218      356 124
#      5:      137        154      182 173
#     ---                                 
# 643422:      331        395      428 180
# 643423:      341        427      526 171
# 643424:      375        427      534 165
# 643425:      345        420      512 164
# 643426:      366        405      530 135
```

<!-- # unique(df_VI_prim$sate) -->

<!-- # unique(df_VI_prim$type_VI) -->

<!-- # unique(df_VI_prim$group) -->

<!-- # unique(df_VI_prim$meth) -->

**column variables**:

  - `sate` : MODIS satellites, one of “combined” “Aqua”, and “Terra”
  - `type_VI`: vegetation indexes, one of “EVI”, “NDVI”, “EVI\_pc” and
    “NDVI\_pc”
  - `group` : 3\*3 grids id, `5` is the central grid
  - `meth` : curve fitting methods, one of
  - `flag` : growing season id
  - `origin` : first date of this year
  - `others` : vegetation phenological metrics extracted by phenofit
