﻿module Day08
open System
open Xunit
open SevenSegmentDisplay

let sampleInput = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""

let puzzleInput =
    """cdbga acbde eacdfbg adbgf gdebcf bcg decabf cg ebdgac egca | geac ceag faedcb cg
edbcfag ebdca ebgad dagbef cfbed adcg dcgeab ac cae cgabef | gacd ac dgac ebdfc
gceafb fcabedg ebfd bd ebacf bafcde daecbg dabfc abd acfgd | adb fagdc bd agfbec
abd cebd bd gcbaf dcafb dface ecgfbda ecdfba fdegca dbafeg | dbecfag bd decb dbgfcea
ecdagfb eb egfcb gcdbfa dcfeg aecb fabcg afebgd ebg agecbf | bge be abcfge bfedacg
dbcf gfc fcbdaeg cf bgfced begacd dbgec gdfea cfdeg bafgce | egcfdb ebdgc gadbcfe bedfacg
efbda dbf fb fegdabc dagbce fcade dfbceg gbadfe abdge gbfa | bafg gbaf bf fb
afbegc bgcfde dc edagf fcadgb gacbf dgc fdagc dgebfca dcab | dc dcg bgcfa adbcfg
gfced ebcg fdgceab gdfbe dfcbea ce dfegba dce fbgecd dcgaf | bceg fbegad ecagbfd dfgec
agbec dgef gfbec ef gfadbc cfgdbe gedfcba cefbda cdfgb bef | bdeafgc gcbdf febdagc fgde
egabc dcgbef efdbac fdaceg af eacbf cagedfb abdf fac bfdce | bedcgaf dabf gfbdaec afebc
dbcf efgdca gacdfb bc abfcg dafcbge gbeaf bac gdecba dgfca | gcfab dbgace abcfg dbegfca
aecbgd bgadf bgdcfea bfgdea egaf gfd eabdg gf adbcf cbgfde | fgea fgd dfg feag
ecdab agcdb adfcegb cadgeb acfdg fgdcbe gbd bg aegb befacd | bgecdaf bgfedca ebdac dacbeg
acfge bcfaedg fb adegfb bcgfae ecfadg fbac fbe ecbfg dgbce | efgbac bfe afegc cbedg
afgec gefbd edcb edcabgf dcg cdfebg fgbade gabfcd edcgf cd | fagdbe gdcbfe gecaf egfac
ce ebadg ebc agfcb cdagbe degbfa edcabf dcge gebac egadbcf | ec afebdg gefbad ec
ebagcf db bfegad gadfb eabfg gebd edbafc dfcgaeb bdf dfcga | cdgaf db adbfegc egdb
gefbcd fgcbd fca gcdfea agfb ecafgdb af dabfc acbde gbfdca | dgbefca bgedcf debfcg gfaebdc
debcf degcfa fdgaeb gcdbea cbga cdbeg decga ebg gb dabcfeg | abgecd bg gcbade baefdgc
gdcbe ga gcae cgdba fgbade fcbedag abfcd gba dacgbe fdbcge | abg dabcg bcfad bga
dagef geab agdfbe dceaf gcdbef egbdfca fabgd eg egf bcgfad | dgafb ge gfdae aedfg
afbecdg cebga abdcge eadcb gbe dbcg befadc fcaeg bg efbdga | gb bfcdae bg bge
dagbcf debfgc aegc bag abgde dbacge dfaebcg bdecg ag adfbe | ga gcaefdb gba bdgaec
abdecf fcdeb dfca bdecg fcb cf febad gbafec dgfbcea gadfeb | dcbfe cfb adfc adfc
dfgcbe dfcbgae dfagb geabdc cfg cgdbf cbfe bdcge cf gdeafc | fdaceg bfec fcbe fdgbc
degfbc ab cfbgade eabc dcbgfa gab cbdega eagdf cegbd abegd | ab dfecbag ba ab
efgac dceagbf faebc efg fg gdeacf gacdeb gbdfea gfdc dagce | fg efacg becaf gfcaebd
adgb befgcd ceafg ebcafd agdfbe fageb gbdfe ba edafbgc eba | ecdgfb befdga badgef bea
fdcabe bdfaecg dage acefg ae fgcba egdcf eaf adgcfe gbcfde | fdceg eaf aef bfadecg
abgdc da gadbec gdbcf dag egbac dcea becfga gafebd dcbgafe | acgbde gdfabe dga dag
cd acd eabdf gadfebc cdaegf beagc bfcd gbedaf bdace bdceaf | bdfc debfac cad cda
geabcf fcg fcaeb aefcgd gfbed defcgab gc gcefb cdeabf acbg | gc dgacef gc geadcf
cbedgf febad faec bfdca ebf ef daefbgc gbead gfcbad eafcdb | gfadbc fe fadcb ef
egfbac fbadec cged dbgfec fde ed abfgd becgf acdegbf efdgb | fde cagfbde ecdg dcge
gbadf bdagef cdgb dbgcaf bc cedfa ebgadcf begfca cab dafbc | bcdfga cfaed cgbd dfcab
fcgdba abdef dbfce efc fbeacdg gedc gbfced gacbef ce fgcdb | afedb fec fce dbfcge
bef cfae adefbc afdgebc fabed fcebdg fdbac fbdgac ef gedab | gfdabec afce fe cbgadf
gabe ceg fgace fbedacg ebafc ge ebgfca gdbcfe gcadf cabfde | dacefb gfcdbe cge becgdf
ebgcad dcagfeb dacg gfabe gbedcf gaedb acbde bcefda dbg dg | gdcbea bdg agedb becdfag
ebfag agdebcf gedcaf cefga gdcbaf efdba acebfg bg fbg cbeg | bgf gceb gfb ebcg
bcaedg ecf dgfe cegad caefg gdbaecf ef cgedfa dfecab bfacg | edcag geacd fec edgf
facdeg gb abfegd eadfg bgfd bcead ebg cebagf gabed gbcafed | dbagfe fgdb aegfd fbdg
aedgcb cdg fgaced afbdg fagec dbagfce egbacf decf gafdc cd | dc gcd cd cgefba
ecbf bcadf afecbd be bed gacfdb gdbeaf cbdea becdgaf aedcg | cdaeg bde bfec dbefca
feacg cfgebd dfaceg adbcg cegfab bagec bedfgac befa eb geb | acbfeg be ceafg fbae
cdgbfa cbfea efga becdf caf gbcaed bgefca af cegdfba agbec | fage fa cfa fgcbea
bdagc fecdb bfeacd gaedfb gfcdbe afd af ecaf edcafbg afdbc | fda fegacbd dgabc fabcd
gfecba fbaecd bagf gcefa gac ag egbacdf dcabeg fecgd efabc | edbcag efacg gafb gacfbe
aecdfb adegf gbcdaf egfdc gfa eagb ga gbaefd gfedabc fadbe | gfedc cdafbe edagcbf edgbcfa
eacbd fbcegad efdcb eafbdg eabcdg cad ca gcbadf gdbae gcea | dac dbegfca cdbea dca
beg bfadeg bfgcde egdacbf ebagf caefg eadb abdgf eb abfgdc | bge eabd dbfgce edab
fadceb cef edac ec efabcg fcdbg befda bgdefac befdga ecbfd | aecd fgeacdb ce ecbadfg
fdagbc fcgebda dcae dgceb abged dc cdb gcebf aefdgb dcbaeg | afdbgce bdgcea ceda efbgcad
dcbage bdefag cafgd cbef bdf cefadb dbcfa bf ebadfcg bcead | bgeacd bafdc fb efcb
fbgac fbg abfcdg gefdac bg efgbda dbgc dagcf bcefgad cfbae | gb afbdcg ebgcadf dgafebc
gedfb fbcea gbeaf gcfeba bdgceaf fedagc gae adcebf ga acbg | ga cdabfe ag cfeab
aefcd efbadc bedc fcgbda deafb adgcfe fdb efbag gafdebc bd | bedgcaf bd bcadfg cebd
adfcge gfadc dcfab gecd fbaedg adegfbc cefag baecgf gd fgd | cdge adfcb cgde gd
ef aef adcfe cdfbga bcfe bcfeda dabfc fagcdbe bedfag adceg | bfdca faecbdg fe dgfabe
gcefdb acgeb dfage dfcegab gcdea edc gdaebf adfgce dc dacf | afdgbe cafdegb dbgcaef aefdgb
afebd dbeag efacbdg bdeafc gfdcab fdge gad ecbga gd adegfb | dga badfgc bedag dag
agfdce ecagdb fdbac fdeca egafd ce gbacfed begfad efgc aec | ecfg ce ce daegbfc
cabged fabge afd facbgd cdgba cfdg fd baecdf bfdgeca gbafd | efcadb dfcg fcadebg cfeadgb
gabfc aefdb adc fcdbae cgdebaf cefgda cbed bgfeda acdbf cd | daebfg dgceaf cd cd
acgbde cdabgfe gae bfcgda badcg baed gbeca cgfeb ecdfga ae | aefdcg bdae eag cgbafde
bfcega fagdc egfacdb efbd dfebag adegb dagfe ef efg begacd | debf cadgf abgecf deafg
gd gdabc gdfa gbd gbdacf cdebafg fcdebg dbcfea agbec cfdba | bdg befdgc acbeg dagf
gbce gef edgcbf cefdga fgabd eg befgd bacedf fdcbe dgcefab | gfe egf cgeb edfcb
eb fcdgb acfge edgcabf fbcaed bfcdge bdge fcbge fbdcga ceb | ecb fcedbag ecb efgbdc
gcea caf fcdebga ac cbdafe gdfbec egfcda dcagf dbafg fgecd | facgd fbcdea ebcgfd decgf
fbca adbcef becgd bcdae acdfgeb cfgdae ab gaebdf abe fadce | fdagec fcebad daceb ab
ca afgbe fdeagcb fabdge gbfac bcgdf bagcef afc dcgafe acbe | ca dgbfc bace afc
dce fgbcead fgecad fedcab cgae febdg fbacgd dfacg egdfc ec | cbegfad ecga efcdg gfcbad
dg gcedbf gdb febdc adbcfe cbgae gfde dgcbe gdacbf bcgafde | fbceda gd cbefad cebagdf
bdg egdfab cgadf adbefc cadbe aedgcb gbacd gcfbdea gb gebc | gbd cbge gceabd gfacd
edgfba cfbgae dfbgace egcfbd efdc cf cgf gdebf fdbgc cdbag | fdgbea cadgb dfeabgc fc
ce bfcdae egfabd cef begc gdebcfa abfeg cfbgae afdgc agcfe | gfabed fec agcbef fce
acfe ecg cdaebg bfgde gbcfe agcfb cfdagb ce agecfb cafgedb | cge face cge fcebga
gbeda baegf dgb dg edacbg afgcdb cdfbae afcdgeb dgce cadeb | dbg debacf bgd edabc
afcdgb fbdceg edcgfa fbc gfbac cbaeg bf gfdca bfad fbcgeda | gecdabf cfb dabf acgfd
af gfcea afg fbgecd bgcafed bgeca gcadef faed cgfbda cgfed | facedg fead edgcaf fdcabg
fcadbeg fag egcfab efadg fbdgce dgacef gedab afdc gefcd af | adgeb gaf afcd afdc
deacgbf bedgaf gbdf fdeba bef bcfage edbca bf fegad dacegf | fdacge edfag cdefga febgac
bafecg cfdba dagcbe aeb cadeg fagecbd deafgc gdbe debac be | ebadgc aeb eb eab
fgad gbadce abd gfedbc cfgeadb ebcaf abdfc fbcdg ad fgdcab | da fdag cedgfb gcbdef
dfcbe cf cef fdgc cdbeg cfdgbe cegdab fgabecd agfcbe bedfa | cef fdgc adfecgb adcfgeb
efcdg afbd bdcafg bfcaedg bdc db bdgaec caegfb bdgcf gfcba | gfdbc db fagcb dcb
bdegfc gd gfacb fcegba fgd cfagd efdbacg edacf adgfbc gbda | edacf gabd gd gd
dfb caebd dfabe abegf fadbeg agcfbe cbadgf df fdaecgb dfeg | bgcdaf fegd df gcbeaf
acfdbe cfd fbeac cd acde gbcadf dgacfbe egfdb fceabg bcefd | eacd dcf gebfd dc
ebg cedfag gb feagd gfdbce begcdaf fagb bdaeg dabec dbfage | fgba bgeadf bedgfa gabf
gdafcbe eag daebf aefdg fecgd ga fegacd cagf debcfg gedacb | cdefga defcg ga gdaecf
dgea gcaebf caefgdb edbcga ced ed cabdef gcabe gdcfb gedcb | daegcfb gdcbf cfeadgb de
cdfage gcbfda cabdfe fcb bf deafbcg aebf bfdec fcead dgebc | fcdageb abdfcge afecgdb eafdgcb
cfgd cg acg bdcagf agfebd acgdb bcdae abgefc gabfd efabgdc | gbcda cbdgefa agc bgdfa
bfcged dbgcf aebcfg cafdebg bcg acdfb gc edgc edfgb aebdfg | cbg cg cbagefd agfebdc
fecdb bcfead bcfda ebacfg fba ab cdabfge dabe adgcf efgdcb | bdea dbfac ba abde
adfgb begc gecfdb ebdafc fbe eb beagcfd efcgad ecfgd gdfbe | bceafd fbe gbfed ebf
gaefdc gdafebc dfba da fgdbe ead bgade agbec edfgab fcegdb | cgbefd ad ead agbec
dceafg fceb afc bcefdag cgdba fcbga eabfcg fc gfaeb ebafdg | debgaf ecbf efbc agbfce
adecgbf aecdbg cgdb aegdb ged gd afdbe fecdag bcefag cbaeg | dg eacgfbd edg gd
dgceb cgafdb cbgea ecgdaf cgaefbd ae gbacf age acegbf ebfa | cgabf faeb fbacg abef
gfec dgbfcae gedcb efb efcdbg ceadbg fe decfb fbgaed dfcab | efb ceabgfd defbga cbdge
efb gdcafe acfbd afcge bgfdec be fcbage gbea bdgecfa ecabf | abeg bef be eb
gbcd egdafc bgcdfe gbf gfedc gfbce faebc gafdeb gacebfd gb | bgcd bgfdae gb bdegfa
cbegf ga fgad agcef aecdgf cag edgbca gabecdf eafcd cefdab | fgcabed ga cga gcfae
badeg ecfdbag gf gaf bfedag bgfad gfeb agecdb facbd cfeagd | agf egdba dbacf gf
abe acdfegb beadgc bcdga caedb dgeb ceabgf be dbgfca cfeda | aeb feabcg eb gdfbac
bcfgd cgaedfb cfegab gd cafbd dgfaeb gecd dfg fbdceg ecbgf | gfceab efgabc cdeg bcgfade
da agcfb edfa egadfbc gcdeaf cegdba agfcd ecdfg gcdfeb gda | adfe fedcgba aedf cfbdgea
cdeabf bcaedgf dcfab bfcag bgdc cegfa gedbfa bg dfcgab gba | gab fgbac fabcd cfeagdb
gad gd daecb bcgdaf adegb cegd eabgf agcdbfe agcebd edcafb | dg edgc dfagcb cged
fbecdg efbagc gacfbd bdfe fe cef fceadgb dgcae cdefg cdfbg | facbeg ebcfgd cadgbf cfbgae
abdce gd gebfcd cadgb gcd bgfeadc adeg adbfec facbg gedbac | gcd dbgaec ebdgca dacbe
gfdcbe adgbfc fedga af bdaeg efca eabfgcd fag fcdge dfecag | cegfbd ceabgdf agf cfae
cf cafbde agfdb cfb bdgcae cebafg acbde bfcda cefd febgdac | gebfacd efabcdg adbcf gdebfac
befcgd aefcbgd fdcg edf adbecg febdc degabf dcgbe fd eafbc | fde edf fcdg df
adef bgdacf caf fgbec fa abgecd acdbe bfeca agbfecd dcbefa | acdeb edaf fdea cbfedag
ceafbg gbacd egacd ea fcdge dabe age cgbdae cfgbad cegbdfa | gdcbea adeb cebgfda dagfbc
bcdeafg gbdafc ed def bfaed dafbg bdfgae bdge fbcea edagfc | dbfeag cadefg de dfe
ebgcdf acefgb ca cfdab cab adcbfe debacfg dfabg adec fecbd | bacfd cba bfgda baegfcd
gceaf afdeg cfaegbd dabge gdf gacfed gaecbf fcgbde fd adcf | fecdgb fd fd dgf
bdfgac bagdc af fadb gfa efgdc bfeacdg bfacge acgdf dgbaec | dgcef aegcbd afg gfedc
aedfbc abdg bceda baegcd cgdefb gb efcga begac efdbcga gbe | aegdcb dbface gedfbc gbe
gdacef deacg fa gdfbcea cfgea bgecf fag cedabg gadbfc dfea | eagcd gbdcaf fga fabcdg
gac afbgc acebf bgdfa gc fbadgce gcefad cgdb dbegaf cabfgd | cg gc bacgdf acg
ebcdg bda dgfacb agebdfc cbdea cbagef da cabdef ebafc fead | ad cgbde cdbeg adef
ed adbfg gbceda gde bgaedfc dgfeb abfcge cbfgde fced begfc | gbfda cgaedfb egcfba begcfd
dfc abcfed cbfaeg adfb cedbf fd bfcadeg cabfe decbg edgcaf | cbefga efcdb gfaecb gceabf
bdegf dbafeg db efagb fcbage bgda decfbag afcebd dbe fedgc | adbefg cdfge ebd gfabe
ag fbagcd fgda dacgb dcebaf abg bcfad efbdagc fecgba ebdcg | gab gcedfba dgecafb fabedc
ebagc bdfaeg ea adec bedcg bae bfgac cdaegb gedcbf dgbacef | cefgbd ea cbdefg bgdec
ecbfd egdfac efagc bcgaefd agcd gfeabd dg cfged gfaebc fgd | acdg dfceb cefadg badfeg
cdfeag cgfea fdgc cf gefdba gecbadf fca cebga gdafe bcfdea | degfbca fcdg bdagfe acf
be fcebga dgefab gdabc egabc febadcg gfcaed ecfb efagc egb | gaefdbc fegadb bfce fceadg
dbagf bcdgfa dbcfg bgaefd bfdaec bcd dc egabfdc gfcbe cdag | gcad acgd dc dbgfa
ecadg cbgad adefbg cdgeba gae gdcabf dfcge ae bace cdbgeaf | ae dagce ebdafgc gacdbf
gdecaf cebdfag fbega bgcfde dcbf gdb cegfd gacdeb db bgdfe | bdg db gbaef dfgbaec
edgacfb cgdfb agdfc dfbeca fdegca ag eagc efdagb agd cdeaf | ecafd dag adg eagc
fg aedcbf bfdac fdgac acdfbg cedga bcdgef gfd fbga acgdbfe | efcdbg dcabegf dgabfce bfadc
efbag cgafb gfdeab eacfdg cbagfe cg gfc gceb dafecgb fbadc | gcf gbfac fbgae gebfa
eacfd dbfaeg cbdgfa badgf efb dfeba fgceadb eb edbg efgbca | ebafgc be fdeab bfe
dbeac bcgfade debafc fdca efcabg gbead bca fdceb gefcbd ac | abc acdbe edagfbc ac
faecd gcbadef fdagc cfg fcbdeg abefgd gacb cdfbga fgbad gc | bcdefag gc gcf dfcbag
dgc dg cbagfe cdgbaf adecgb bacedfg cbaeg bcdeg cefdb dage | gdae cadbge cdg gdc
gfeacb begafcd fgbacd cbafd fbcaed cfead eaf ebad cdfeg ea | cgbfaed dacfebg gcdef cgedf
acgfebd fedbag dbceg badgec gcda efcbg dce bdaefc dc bdage | aebdfg abdeg edc gcad
bafcge bgade gafdb dafbce ebd fadcebg eagcb agcdbe ed gdec | cegbaf cgebda ed edbag
bgfadc bdea fegcbda fgdceb gceaf cabdge gcdeb gda cdgea da | adg edabfgc dfacgb ecfag
efbgdac fceag fdegba gadfbc debc be dcgbea cagbe abgdc egb | dcbe eb efgcbad cgeaf
dabcef dacgfb fgcbe efd ed ecfbgda cdegf dgfeca daeg gfcda | cfgdea gdabcf de de
gcdb ebagfdc fcdag bdfaeg badcgf gdeafc caebf gb fgbac abg | fcagdbe gb bgdc afdgc
acdgf beda gdb bfgade gbfea efbcgd fgdab cgefdba db cfgeab | bfaedg afgdb bdg bd
cgaef eb abgec fbgadc gebd becdga faedbc aeb bdagc dgecfba | dcfabg gcfea bfeacd eb
gbcad fgceab fcaeb faedcb dfbe cdafb cegadbf cagdef dfa df | fgecad efacbd egfcda defb
gdcaef ebafdc bgda abfgecd ebfda ag gae gfdbae begcf gfeba | agbef ga feabg ega
aebcfg dgafc ba dbfge afbdg cabdefg dceagf abg dcba dfcgab | cbefgda dfgeb cgdfae bgafce
ed fcegba gdae adbcef fdcbg gefdbca egfca afdceg dec ecdfg | ecd gfbdc bcfgd dgfce
agdc ecdbagf gbcfa gfcbad dcbefg adfgb ecfbga dbg dbefa gd | bdg facbdeg bdg gdcbaf
db gfdeb ebd adbefgc fgced dfcgae bcegad egcfbd afgbe cdfb | aefcdg fcdgbae egdcf cgeabd
egcfa cefdag cb cgbfea bfca ebdgcf cbaeg eadbg gcb ecdfabg | ceagf cedbfg bacf bfac
gfadcb aedc febcg cd dfecga fcd fegcd gefbda eadfg bdcfeag | aedfg fgaed cd fbecg
adefb gaecf aefdbgc acfdeb cdabgf gb gbed bdgefa bga efabg | dbfae dgabcef abg cbgfda
degcfb adebgc afegdbc ca dfgae acgb degac cdfaeb eac debcg | bacged gadef gbac gbecadf
abgcdfe ace fcaeg fgbac dafe deagcb afgecd ea bcgdfe gdcfe | ea gaebdc ecafdg aefd
cafedbg cdbafe dcfegb abd dfcab gabdef cabe efdbc ba gfcad | ba ebac bad aefgcdb
edbgfa bacdf adfbce caeb efcdbag cebdf bdgfac eb cegfd efb | ebf bfeadc eb bfdeac
cbadfe fcg cbafe fg gbecd fdacbg efgbc fecabg beadgcf efga | abdfceg ecbdg fgceb gf
gbdfa gfceda def befg caebd bdfea fe bfadcg dbfgae dabgcef | dcbea fe efgb bcdea
bacgd gade cdebgf badgce da cedbg dab bfgac egdacfb debfca | gafbced abefcgd da deag
acbdfg fd aecdfgb ecadf dgcbae cedfag aecdg abefc egfd daf | afceb dfa fgde aefcb
cdgafb dcafg afegc gabd bdefcg cgd gfaebdc fbdeac gd cafdb | dg dg gcd dgacfb
geafdc gecfb gafcd gbadfec db bgfeda gbdfc abdc fdcgab bdf | eagfdb bgaefd db bdf
acdf adfge gefcab defcga df fgcae fed bcafdge abdge cgdbef | fd fd gfbaecd facd
cd cebfd defacg fcd gefcdab abdef bceafd abdc befgc dfagbe | bdca dfecb feacdg cfd
gadef fedc eafgb fd ecadg cdabfg fdg gceadb cedgfa edbgafc | bdcagf df egacd bgaecdf
dacbefg ebdcgf cegfda fdcga bfc bfga abcde bcgadf bf bdcfa | fgba ceafgd bf defbcg
ac efcbd gedfca ace febcad cabde gcdbef bgade bdegacf cfab | befcd acbf ceagbfd abdecgf
dbfagc abdce fgea gab ceagb fbcega gecdfba ag gfecb efcgbd | cgadfb aefgbc gaebfc efag
cebfd bdfeag fgc edgaf gc dgca cdebafg gcfde geafcd feacbg | fcgde gcdeafb adcg gfdaceb
gcebadf cebdag egcfda acfd cdegf fgecba cf dgeac fcg fdgeb | decfga fdac egabcf ecabdg
fgabc egbfcd eaf gead dfegb edgcbaf agfdeb acbdfe ae afgeb | gdebf dgae gebdf efa
gcadbe bacgd gcbafed gdcef aebfgd age aecgd cabe dfbgac ae | dcgba bace age acbe
fgdca gcabfd dfea gcefa ecdgaf adfbgce cae gecabd cgebf ae | ae fcegb acgdebf acbedg
acbdgfe gdcfa aecfg abfdc gd fgdb efbacd gfdbac gda bcedag | dga fgdb bagdcf gd
fg dagfcb gfeac cegba dfceab adfgbec fged gfacde fcdae fcg | dfge fgc acgef gcf
agfcbd cbagef edbfg ecbgafd bagec fageb cabdeg fa ecaf abf | efagb debgf af caebgf
afecdg decgfb geabd bgd afbd bd caegb fgacbed feagd gfaedb | gcafed cebfdg fadb cfaedg
cedfb dgfeab ecgba fcdg fgb befgc fdaebc gfebcd fg adcgfbe | fdgc afbdcge cgdbef cefbg
defc gebcf cgdbfa fc cegab bfdeg dgcfeb fgc bgadfe dagcfeb | fcde bedgf gfc gfdbea
badceg bea gbafc dafge cfbdga geafcb cfbe gabfe eb gfcdabe | eba bfce fbec eafcgb
fbcedg gf aedcg decbf gfeabd efg gbcf faebcd eadbgfc efdcg | fge dgcbef fge cfgb
cbdeg bfca ca bgefa aebgc fcgaed gcfbea gac cfegabd defgba | gcaeb gebfda bgedfca ca
ecfd cabdg bcfeda cdeab ed eafbgc dae gaebfd ceafb cfgdaeb | cefadb befacgd ecfd gbfaced
cgdea gcfabe afecgbd becgdf eb abgce abef bce fcgab dcbfga | bgfdce egcfbd ebcfgd ebcdafg
fcbed fbged fdag gd egdfbac gacdeb egbaf dge cbeagf fdbgea | deg dg dacegb cdgabe
faegcbd aed gfdba fdce gfacde ed adfge dcegba acgebf fcaeg | aedfg cebagf de de
dbcega geabf gcebfd deg cfbade dg debgf dgfc dcfbe ebadfcg | cdfaeb deg gfabe gcbedf
bgecd bafg efdab ebdcaf edfgab gf dfg degbf fcgeadb aefgcd | gabf bdgfe fbdae cbedfa"""

let firstLine =
    getLines sampleInput |> List.head

[<Theory>]
[<InlineData("fg", 1)>]
[<InlineData("cgb", 7)>]
[<InlineData("cgba", 4)>]
[<InlineData("gbdfcae", 8)>]
let ``Identify digits``(input, expected) =
    let digit = createDigit input
    let result = identifyDigitValue digit
    Assert.Equal(Some expected, result)

[<Fact>]
let ``Look for digits by unique segments in sample data``() =
    let result = identifyAllOutputDigits (getLines sampleInput)
    Assert.Equal(26, result.Length)

[<Fact>]
let ``Look for digits by unique segments in puzzle data``() =
    let result = identifyAllOutputDigits (getLines puzzleInput)
    Assert.Equal(452, result.Length)

[<Fact>]
let ``Deduce display configuration``() =
    let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab" |> toDigitList
    let top = figureTop input

    Assert.Equal(top, "d")
