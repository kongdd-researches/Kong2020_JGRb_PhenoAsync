## multiple growing season sites
# c("BE-Lon", "CH-Oe2", "CZ-BK2", "DE-Geb", "DE-Kli", "DE-RuS", "FR-Gri", "IT-Ro1", "IT-Ro2")
get_gsl <- function(d, 
    by = c("sate", "type_VI", "type_period", "meth", "site", "group", "origin"), 
    melt = TRUE, 
    value.name = "y_obs")
{
    by  = intersect(by, colnames(d))
    res = d[, .(
        TRS1.gsl = TRS1.eos - TRS1.sos, 
        TRS2.gsl = TRS2.eos - TRS2.sos, 
        TRS5.gsl = TRS5.eos - TRS5.sos, 
        TRS6.gsl = TRS6.eos - TRS6.sos, 
        TRS8.gsl = TRS8.eos - TRS8.sos, 
        TRS9.gsl = TRS9.eos - TRS9.sos, 
        DER.sos  = DER.eos  - DER.sos, 
        GU.gsl   = RD - UD, 
        GU.gsl2  = DD - SD, 
        Zhang.gsl  = Dormancy - Greenup, 
        Zhang.gsl2 = Senescence - Maturity
        ), by]
    if (melt) {
        melt(res, by, variable.name = "index", value.name = value.name)
    } else res
}
