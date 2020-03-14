
25420229
paste0(getwd(), gsub('[.]', '', sRawXMLPath),
       grep('25420229', all.xmls, value = T)[1])

f <- '/home/light/git-repos/zakupki_gov_ru/data/raw/10_from201901to201912_loaded2020-03-06/xmls/fcsProtocolEF1_0355100010919000101_25420229.xml'
rootNode <- xmlTreeParse(f, encoding = 'UTF-8', useInternalNodes = T)
# танцы с namespace
nsDefs <- xmlNamespaceDefinitions(rootNode)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
names(ns)[names(ns) == ''] <- 'xmlns'

path <- '//xmlns:application/xmlns:admitted[../xmlns:admissionResults]|//xmlns:application/xmlns:admitted[not(../following-sibling::xmlns:admitted)]|//xmlns:application/xmlns:appRejectedReason[1]/xmlns:explanation'
path <- '//xmlns:application/xmlns:journalNumber'

xpathSApply(rootNode, path, xmlValue, namespaces = ns)

DT


