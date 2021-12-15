﻿module Day09
open System
open Xunit
open FsUnit.Xunit
open SmokeBasin

let sampleInput = """2199943210
3987894921
9856789892
8767896789
9899965678"""

let puzzleInput = """9756789545987652345891012367899989991359999866435974321921234989994346679898799998765498765435697654
6545898959876541236789245458989878989498987654323965639892949878989134569656678997654349979856789543
5434567899765430445678997969876569878987699543219896798789894365678945698744567998765134598767899432
6545678932984321267789789891997678967896598759909789987676789234789659987632456789654356789878978921
7657789793965432457894698790198989456789479898898667896555678945678978997543867898785467893989567892
9869895679876566568923987689249992365892357987676557899434567896989989987656789939996579932193456943
4989954234989878679019876578998991234595456796543434678923678999898799998789899123987989521012397894
3496543129998988789329876439977789345789597987632129789212349998789698989897978994598995432426789965
2129692098987599998998765329865678968997678998549098994323567987678987678976665989689876543534999876
3098989987595434567899876434974567899398789987698987896434989896557996567895434878999987854545678998
5987678976434323568967986549865678943239892098987676987976898765346799678994323469439998966656799109
9876567897321012678957797656976899655399989999876545898987979943235678989986554568998999987767988912
9965478989432324589345698898989938966989978899989656789399865432133788993297676689987894398979877894
9854365679645465678956789999990127899879867789199987891245998943012456789198787998656789239998656789
8743234568956977899767998788954356998765945691019998943356987875123869891019899876545678945987545689
7654565699967989998978987687895968998954323989198789659767996543234789999999939867634569999876434799
8765776789878998767999096556999899987643219878987698798999898865345679987889129954323456789987545989
9878987895989987656789195348986789398764329767976549987876799876456789996678998765434667896987659978
4989298934599999732678989199875995499875498656896539876545689998767999954567899878545789945699798767
3494349210298797653567967988954896989976987645689321987436578999898999893456899989876893434923989856
4599959921298598787679459976543789978999876434589210976523459899999976742347978992987942129899876432
5987897893987329898796567987665679567999654323568929897735598789998765431234567893999891098789765421
9996456789876534999897698998987789459898721012478998789896698678999976210123456999898789997659875432
8965347899989676799998999239898891298765432153459987678987966539987894323234567896789679898545987644
7996756789499897989989987546789920989878543264567896569999854324456789534345678965394598765434698756
6789767892349989878975599767896439876989765345688923478999784213345678946456799876213459876723459887
5678978921298976567894409898989598765699987656789212599987652101234789987567899764302398785412345998
4449899910987665456793219979878997954569998987897423689998543212345896599878999543213987654324456789
3239789899876534367894998764769886743467899498986564799987656423456789445989398765454999785457567899
4598566798954321256789869843459765432356974329299875678998767634567995234799209876579879876569678998
5986454677894310345898954994569874321678965410123986789469878747679654345678999997679865987689789877
6797323476989321238997899989689987434589654321244597893342989656789765467889989998989996898999898756
8983212345678932346986997879999876565698765434456798932101298977899876988999878999993987919999959843
9975903559789994569975986567896987677899876545587899653214397989989989899019767898912398909898943232
4599899678999989798754578456955698988923997658698998794329976595678998789129654567893459998767892101
5987788989999878998543462349543459899019898779789989989498765434569987679398743979999767897655589312
9876567899898767987662101268942398789298629989899877568998755323498656568999999890198978998543478923
9895456798786545698778232358931987699497545699999865456977644212987545457889878789987899987632767894
8754234596565434797654345667899876568987676789988784345965432109876532345678965678976789876521256789
9764345789432123459765456779999875457898789999879651299879645213989921237899234569895789765430138894
9865756896543016569876767893498766346789894345965432989998759324599820126921012379784698654321239995
2976787998652127678988878932398754237899953239876544979329878435798762245933126498653789785434346789
1297898999763346789399989901249832126989762123987959868910987656789954349894598989762239896565656994
0129969899874557895212999899498744335678921014599898957891298767998766498789679876510147997677878923
1998654698986667894329896788987656567899432345698767545792999878989878987698989989324235698988989212
9887643567898989965598765677598767779988943456796653438689879989678989876567891098945356789799999909
8798432345999298978987654356459878989767894677985432123578968994568998797456992987897459895678999898
9654321456890197899097563234346989995456795989877321017679356795679999656356789876789567954567898767
8765430127789986792198432101245699876345899898765432128793245789989987943237998765698978993498987545
9876541235699975679239983432367898765456987659876543234892125989999986542126789654567899689999896536
2987678346988854568949876543478949876597998767987654356943014579989876531045897543456912567898789747
1298898769876743457899987654579134987678939898998765467897123468967987432236986421257893478967678958
0199959899765322346789799867898995999789023999999887678995434679656798543987895432348994589656569899
9986545998754301258897658978956789879895235699988998789987556989769899864598976553656789696545456789
8765434459643213478976546989345678967994345789767899896898678999898989765899987698767898789434345898
9876321398954344569987697991236789456987656799856789954779789768997679876789298789878999899321234987
2995456987896476778998989754347894312398768987545878923569896659876545997894349996989999998939359876
1987569876789569899249878965456965323469879876534569012345964345987432198989498765699989987898967965
0198678975679978954399867896567899434599989987545679923469876239764321019678987654569979546757799654
3239989764567899967976556899778978949978993098656897896598754349989433124599998765798767632345678943
4599899865698999879865445798989767898767892129789976789679875698998765355678999879987654341296799432
5988767978969789998765334567893458989656789399899765678989996987659889467789899998998793210989989949
9876543989656678987654125678942349876545679989998976789699999876546996578996799987999989349878567898
6565432391234569998543276889543456985431098876767897899598799988632987689545678956899878998767479967
5432101239345899998667989997675669876545987854356789968987689999991998795435789445698767999843234556
8556214398996799898778999998787899987659876543248895459876578999889899899556895334989656789654012345
7679345987889988759989978899898999999878997652101979398765456798766789998787954129876545699964144569
8798956986679876543598767698999998889989019654214568998654334989655679219899543012965434759893234578
9987899765498765432489656597899987679993198784323679876543129876544598923965432129877521238789345679
9765978964349876421278945456799876568943239875634789988651012987623467899987653235999430145678968789
8964568991256995432359932345679987459855945987845899997762129875434568978998864356986543236899879897
7843456989346986548767891234878996598769896898956999876543434986645779767879985467897856545999989986
6532345678967987669878910123567897679898789999767899997655645698756898658769876778998987656789997675
4321234789879299879989892344579999999987689989998999898966758789867987845459989899899199787899896543
3210345897989129997998789456689988878996569879329998789897969895978976432348994966799019899978795432
4321296896591097856897678967789876569895454768910987656799892924989986521467893245678929956965679321
6542389989432976545734577899893985498764343456891298545698791012597895430349964346799998767894578910
9543578978949865434823456789932987679863212347892999935797689133456789421258975959899989878943467891
8754699567899876321012567893201298789756101356789899896986578994567899532367976778987975999892348989
8767976456912986542153457954314349897543212467896798789876459789688998943489997889676899976790199878
9898995345893498543464768965326456999656353478945987678954345678999987894598789996545998765891989767
3919894236789987655575889876539567898767764589439876569895234589998776789679565899876989876799875656
2109789129894298967989999989798978999978879789520995436789125678999654568989434789987976987987684545
3998679298932129878996899998987899999989989997621986545678976789898768679997645678998965799896543334
9877568997893245989345678997676798989993498765439997876789499895699879789898796789769876898789932123
9965479876789346790123789976585987678891989976998998987896321934987989896789989999953987987667899014
7654398765695456989545699985434976567789878989897849798965490129996793935679879899892098998545678923
8743219754989579879956789496929895434678966799765434659896989298765432123569767676789159995534567895
9854598853878998767897992349899789323789655679876210346789979349896541012998756545679234974323567896
8765997542367889658989321098785678934578934567985421235899767956989432129876543236799399863212455689
9879876521345678947678932297654569965989312399876534569967656797977993234997432145678987654201234678
5989854310456899434567894598765789876792105789997945678954546989865789945696549236799398765629346889
4198764321697974325778998679986993989893234578999896989543535976984567896989798947893219876548956789
3019878932789765534568998799987894599954345699998789895432123495432378999878987899942101987656787895
2123989549898987689679439989998965698765456789987678789594345989543489998769556678943219899987998964
4257898678977698789989549878999878899876977899996545678975459878954599987654434569654598769999459995
4345998789764569897999699767899989967987988949876434989986969767895678998763123678967987658954356789
5656799893212698975778987656787896459898999235986545795499899656987789349874564989989996547893235889
6767899965435987654569876544656789598769210124598678894298798746899893298765678991095987656792023979
9898999876786986543210987632347899987654321235698789965987653234901910129886799543234599767893434568"""

[<Fact>]
let ``Read input into heightmap``() =
    let result = readInput sampleInput
    let expected = [|2;1;9;9;9;4;3;2;1;0|]

    result.MapValue[0] |> should equal expected

[<Fact>]
let ``We can index into middle of heightmap``() =
    let result = readInput sampleInput

    result.MapValue[1][1] |> should equal 9

[<Fact>]
let ``Get neighbors for item in middle of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (1,1)

    let expected = [3;8;1;8]
    result |> should equal expected

[<Fact>]
let ``Get value from coordinates``() =
    let heightmap = readInput sampleInput
    let result = getValue heightmap (5,4)

    result |> should equal 6

[<Fact>]
let ``Get neighbors for item at top-left of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (0,0)

    let expected = [1;3]
    result |> should equal expected

[<Fact>]
let ``Get neighbors for item at top-right of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (9,0)

    let expected = [1;1]
    result |> should equal expected

[<Fact>]
let ``Get neighbors for item at bottom-right of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (9,4)

    let expected = [7;9]
    result |> should equal expected

[<Fact>]
let ``Get neighbors for item at bottom-left of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (0,4)

    let expected = [8;8]
    result |> should equal expected

[<Fact>]
let ``Use neighbors to check for low point``() =
    let neighbors = [1;2]
    let value = 0
    let result = keepLowPoint neighbors value

    result |> should equal (Some value)

[<Fact>]
let ``Use neighbors to check for low point not being found``() =
    let neighbors = [2;4;4]
    let value = 3
    let result = keepLowPoint neighbors value

    result |> should equal None

[<Fact>]
let ``Search all lines for low points``() =
    let heightmap = readInput sampleInput
    let result = getLowPoints heightmap

    result |> should equal [1;0;5;5]

[<Fact>]
let ``Get sum of risk level``() =
    let heightmap = readInput sampleInput
    let result = getRiskLevelSum heightmap

    result |> should equal 15



[<Fact>]
let ``Build basin locations``() =
    let heightmap = readInput sampleInput
    let result = buildLocation heightmap [] (0,0)

    let expected =
        {
            Height = 2
            Position = (0,0)
            Top = None
            Bottom = emptyLoc (0,1) 3 |> Some
            Left = None
            Right = emptyLoc (1,0) 1 |> Some
        } |> Some
    
    result |> should equal expected

[<Fact>]
let ``Create basin from location``() =
    let location =
        {
            Height = 2
            Position = (0,0)
            Top = None
            Bottom = emptyLoc (0,1) 3 |> Some
            Left = None
            Right = emptyLoc (1,0) 1 |> Some
        } 
    let result = createBasin location
    
    result.Positions |> should equal [(0,0); (0,1); (1,0)]
    result.Size |> should equal 3

[<Fact>]
let ``Find all basins in sample input``() =
    let heightmap = readInput sampleInput
    let result = findBasins heightmap

    result |> List.take 3 |> List.map (fun x -> x.Size) |> should equal [14;9;9]

[<Fact>]
let ``Score via top 3 basin sizes``() =
    let heightmap = readInput sampleInput
    let result = basinScore heightmap

    result |> should equal 1134

