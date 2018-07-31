
'copy "G:\My Documents\GitHub\zakupki_gov_ru\data\raw\201709-201805_15-06-2018\xmls\"'
'"G:\My Documents\GitHub\zakupki_gov_ru\xml_examples"'


flnm <- './xml_examples/fcsProtocolEF2_0111100000217000020_15535590.xml'

rootNode <- xmlTreeParse(flnm, encoding = 'UTF-8', useInternalNodes = T)
# танцы с namespace
nsDefs <- xmlNamespaceDefinitions(rootNode)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
names(ns)[names(ns) == ''] <- 'xmlns'

path <- '//xmlns:application/xmlns:journalNumber[../xmlns:priceOffers]'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)




flnm <- './xml_examples/fcsProtocolEF1_0111100000217000017_15516465.xml'

rootNode <- xmlTreeParse(flnm, encoding = 'UTF-8', useInternalNodes = T)
# танцы с namespace
nsDefs <- xmlNamespaceDefinitions(rootNode)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
names(ns)[names(ns) == ''] <- 'xmlns'

path <- '//xmlns:application/xmlns:journalNumber'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)

path <- '//xmlns:application/xmlns:admitted[../xmlns:admissionResults]'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)




# flnm <- './xml_examples/fcsProtocolEF1_0111100000217000020_15516470.xml'
# flnm <- './xml_examples/fcsProtocolEF1_0311100006117000056_15221616.xml'
flnm <- './xml_examples/fcsProtocolEF1_0111100008317000293_15799266.xml'

rootNode <- xmlTreeParse(flnm, encoding = 'UTF-8', useInternalNodes = T)
# танцы с namespace
nsDefs <- xmlNamespaceDefinitions(rootNode)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
names(ns)[names(ns) == ''] <- 'xmlns'

path <- '//xmlns:application/xmlns:journalNumber'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)

path <- '//xmlns:application/xmlns:admitted[../xmlns:admissionResults]|//xmlns:application/xmlns:admitted[not(../following-sibling::xmlns:admitted)]|//xmlns:application/xmlns:appRejectedReason[1]/xmlns:explanation'
path <- '//xmlns:application/xmlns:admitted[not(../following-sibling::xmlns:admitted)]'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)



# flnm <- './xml_examples/fcsProtocolEF3_0111100002017000012_15210165.xml'
# flnm <- './xml_examples/fcsProtocolEF3_0111200000917000706_15317307.xml'
flnm <- './xml_examples/fcsProtocolEF3_0311300012718000021_18772833.xml'
rootNode <- xmlTreeParse(flnm, encoding = 'UTF-8', useInternalNodes = T)

# танцы с namespace
nsDefs <- xmlNamespaceDefinitions(rootNode)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
names(ns)[names(ns) == ''] <- 'xmlns'

path <- '//xmlns:applications//xmlns:journalNumber'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)

path <- '//xmlns:application//xmlns:appRating|//xmlns:application/xmlns:appRejectedReason[1]//xmlns:reason|//xmlns:application/xmlns:notConsidered'
path <- '//xmlns:application/xmlns:notConsidered'
xpathSApply(rootNode, path, xmlValue, namespaces = ns)



