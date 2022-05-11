library(VennDiagram)
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

set_std <- list(A = c(1:70), B = c(68:72))

set_inclusion <- list(A = c(1:70), B = c(1:3))

set_coincidence <- list(A = c(1:3), B = c(1:3))

set_exclusion <- list(A = c(1:40), B = c(41:50))

vd_std <- venn.diagram(set_std, filename = NULL,
                       euler.d = TRUE)
grid.newpage()
grid.draw(vd_std)

vd_inclusion <- venn.diagram(set_inclusion, filename = NULL,
                             euler.d = TRUE)
grid.newpage()
grid.draw(vd_inclusion)

vd_coincidence <- venn.diagram(set_coincidence, filename = NULL,
                               euler.d = TRUE)
grid.newpage()
grid.draw(vd_coincidence)

vd_exclusion <- venn.diagram(set_exclusion, filename = NULL,
                             euler.d = TRUE)
grid.newpage()
grid.draw(vd_exclusion)
