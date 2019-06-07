### Some edits I did for the original excel file: 2013-2018 Pitch Orders

Firstly, I merged the different rows of the columns:
ord\_OIT\_\_OrderItems::Description, ord\_OIT\_\_OrderItems::Price,
ord\_OIT\_\_OrderItems::PriceBasisCode,
ord\_OIT\_\_OrderItems::ProgEventCode into one row in order to put all
information of each customer in one row to make further analysis easier.

Then, I deleted the other rows which have already been merged into the
first row of each customer.

The above step makes 2,351 rows into 941 rows.

Thirdly, I created a new column: FullName to merge the FirstName and
LastName.

Finally, I filtered the column: ord\_OIT\_\_OrderItems::Description
which contains Lone Star Badge, Pitch Competition. We can find that
there are 105 rows which contains Lone Star Badge, Pitch Competition out
of total 941 rows.

The new excel named LS\_PC\_Customers that I edited is attached in the
email.

### Report

After looking into the data, there are totally 97 people who bought Lone
Star Badge and Pitch Competition Ticket from Year 2013 to Year 2018. To
be more specific, 91 people only bought the above tickets once, 5 people
bought twice, 1 person bought three times. The detailed information
including Main ID, order ID, Full Name and Item Description.

#### The Whole Name List for People who bought Lone Star Badge and Pitch Competition Once, Twice and Three Times

    ##                 FullName counts
    ## 1               AaronMay      1
    ## 2         AdrienneHarmon      1
    ## 3         AllisonHerrera      1
    ## 4              AmySirizi      1
    ## 5           AndreaDelott      1
    ## 6             AndrewGray      1
    ## 7             AniJarrett      1
    ## 8           AnnaBierhaus      1
    ## 9            AsadDurrani      1
    ## 10       Avery HThompson      1
    ## 11            BillThomas      1
    ## 12           BrianSchwab      1
    ## 13       CandraBohjalian      1
    ## 14       Carl CotaRobles      1
    ## 15          CarlishaBell      1
    ## 16 Cath SwanstonCampbell      1
    ## 17       CharlotteShafer      2
    ## 18          CindyWeigand      3
    ## 19        ConnieAnderson      1
    ## 20         DanaleeBuhler      1
    ## 21         DanielDerksen      1
    ## 22            DanielKorb      1
    ## 23           DavidDorsey      1
    ## 24          DavidWheeler      2
    ## 25        DeangeloManuel      1
    ## 26            DennisMonn      1
    ## 27            DruSellers      1
    ## 28       DustinHennessey      1
    ## 29            ElaineTise      1
    ## 30       ElisabethMisner      1
    ## 31       ElizabethSchaaf      1
    ## 32        ElizabethWheat      1
    ## 33         GarrettGraham      1
    ## 34            GiaLomenzo      1
    ## 35         GloriaBankler      2
    ## 36        GregHovanesian      1
    ## 37          GretchenCion      1
    ## 38  Hans-PeterZimmermann      1
    ## 39             HelenLemm      2
    ## 40            JackieLang      1
    ## 41         JacklynThrapp      1
    ## 42          JaneMcMackin      1
    ## 43          JanetKilgore      1
    ## 44           JayBoisseau      1
    ## 45            JayMatthew      1
    ## 46        JenniferMedvin      1
    ## 47             JessGrant      1
    ## 48        JessicaFerrato      1
    ## 49            JiefeiYuan      1
    ## 50             JohnClark      1
    ## 51           KaraBarrett      1
    ## 52          KarenRessler      1
    ## 53       KatherineKrasin      1
    ## 54      KathleenMcDonald      1
    ## 55           KellieOlive      1
    ## 56            KentProbst      1
    ## 57        KimberlyBrooks      1
    ## 58      Kimi BuserClancy      1
    ## 59           Kirk Manuel      1
    ## 60         KristiFrazier      1
    ## 61   Laura MesserJackson      1
    ## 62            LeighLewis      1
    ## 63 Leslie SimmonsSimmons      1
    ## 64     LynnVincentnathan      1
    ## 65         MalcolmWeekes      1
    ## 66         MarciaGleeson      1
    ## 67          MariaRublein      1
    ## 68       Martha LynnCoon      1
    ## 69            MaryKaiser      1
    ## 70            MaryTrouba      1
    ## 71          MatthewNoble      1
    ## 72            MeghanRoss      1
    ## 73       MichaelStreiter      1
    ## 74         MitchellEmoff      1
    ## 75          Miwa DeSilva      1
    ## 76        MollyMollotova      1
    ## 77           PaigeGibson      1
    ## 78       RachellMartinez      1
    ## 79   RachelWilkins Patel      2
    ## 80           RayUzwyshyn      1
    ## 81          RichardWarke      1
    ## 82         RobertReichle      1
    ## 83         RobertTartell      1
    ## 84            RobRaffety      1
    ## 85         RoseanneFurst      1
    ## 86          RubenRamirez      1
    ## 87            RyanBarlow      1
    ## 88       SaikatMukherjee      1
    ## 89           ScottDobbie      1
    ## 90         ScottieRitter      1
    ## 91         SheriChandler      1
    ## 92         SriAppalaraju      1
    ## 93         STANLEYBARTON      1
    ## 94       StanleyLombardo      1
    ## 95           StevenDente      1
    ## 96       Terrence CMason      1
    ## 97          YvonneMedley      1

#### People who bought Once

    which(df$counts == 1)

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 19 20 21 22 23 25 26
    ## [24] 27 28 29 30 31 32 33 34 36 37 38 40 41 42 43 44 45 46 47 48 49 50 51
    ## [47] 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
    ## [70] 75 76 77 78 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
MainID
</th>
<th style="text-align:center;">
OrderNo
</th>
<th style="text-align:center;">
FullName
</th>
<th style="text-align:center;">
Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
79325
</td>
<td style="text-align:center;">
16-1049
</td>
<td style="text-align:center;">
YvonneMedley
</td>
<td style="text-align:center;">
Awards Luncheon 15 Oct (Sat) 12:00pm , Lone Star Badge, Capital Badge,
Producers Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
79065
</td>
<td style="text-align:center;">
16-1128
</td>
<td style="text-align:center;">
JohnClark
</td>
<td style="text-align:center;">
Film & Food 12 Oct (Wed) 7:00pm ,Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
99350
</td>
<td style="text-align:center;">
18-1326
</td>
<td style="text-align:center;">
PaigeGibson
</td>
<td style="text-align:center;">
Film & Food,Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
96081
</td>
<td style="text-align:center;">
18-1618
</td>
<td style="text-align:center;">
LynnVincentnathan
</td>
<td style="text-align:center;">
Film & Food,Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
50006
</td>
<td style="text-align:center;">
14-1048
</td>
<td style="text-align:center;">
MitchellEmoff
</td>
<td style="text-align:center;">
Film Pass, Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
78222
</td>
<td style="text-align:center;">
18-1700
</td>
<td style="text-align:center;">
MeghanRoss
</td>
<td style="text-align:center;">
Film Pass,Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
48472
</td>
<td style="text-align:center;">
13-0670
</td>
<td style="text-align:center;">
Hans-PeterZimmermann
</td>
<td style="text-align:center;">
Lone Star Badge, Conference Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
60555
</td>
<td style="text-align:center;">
14-1165
</td>
<td style="text-align:center;">
StevenDente
</td>
<td style="text-align:center;">
Lone Star Badge, Conference Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
58562
</td>
<td style="text-align:center;">
14-1197
</td>
<td style="text-align:center;">
JenniferMedvin
</td>
<td style="text-align:center;">
Lone Star Badge, Conference Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
57092
</td>
<td style="text-align:center;">
16-1352
</td>
<td style="text-align:center;">
JessGrant
</td>
<td style="text-align:center;">
Lone Star Badge, Conference Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
40931
</td>
<td style="text-align:center;">
13-0105
</td>
<td style="text-align:center;">
ConnieAnderson
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
50553
</td>
<td style="text-align:center;">
13-0646
</td>
<td style="text-align:center;">
JaneMcMackin
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
50566
</td>
<td style="text-align:center;">
13-0674
</td>
<td style="text-align:center;">
KimberlyBrooks
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
15092
</td>
<td style="text-align:center;">
13-0717
</td>
<td style="text-align:center;">
KatherineKrasin
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
53478
</td>
<td style="text-align:center;">
13-0824
</td>
<td style="text-align:center;">
Cath SwanstonCampbell
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
9067
</td>
<td style="text-align:center;">
13-0832
</td>
<td style="text-align:center;">
MaryKaiser
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
50883
</td>
<td style="text-align:center;">
13-0925
</td>
<td style="text-align:center;">
Avery HThompson
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
51877
</td>
<td style="text-align:center;">
13-1180
</td>
<td style="text-align:center;">
Martha LynnCoon
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
60191
</td>
<td style="text-align:center;">
14-0744
</td>
<td style="text-align:center;">
KarenRessler
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
62124
</td>
<td style="text-align:center;">
14-0981
</td>
<td style="text-align:center;">
JiefeiYuan
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
59102
</td>
<td style="text-align:center;">
14-0982
</td>
<td style="text-align:center;">
AnnaBierhaus
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
63930
</td>
<td style="text-align:center;">
14-4648
</td>
<td style="text-align:center;">
SriAppalaraju
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
69954
</td>
<td style="text-align:center;">
15-0841
</td>
<td style="text-align:center;">
Kirk Manuel
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
70227
</td>
<td style="text-align:center;">
15-0842
</td>
<td style="text-align:center;">
DeangeloManuel
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
70381
</td>
<td style="text-align:center;">
15-0897
</td>
<td style="text-align:center;">
Carl CotaRobles
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
71167
</td>
<td style="text-align:center;">
15-1150
</td>
<td style="text-align:center;">
JanetKilgore
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
71209
</td>
<td style="text-align:center;">
15-1225
</td>
<td style="text-align:center;">
ElizabethWheat
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
69550
</td>
<td style="text-align:center;">
15-1258
</td>
<td style="text-align:center;">
KaraBarrett
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
78749
</td>
<td style="text-align:center;">
16-1283
</td>
<td style="text-align:center;">
MatthewNoble
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
77381
</td>
<td style="text-align:center;">
16-1305
</td>
<td style="text-align:center;">
GregHovanesian
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
14929
</td>
<td style="text-align:center;">
16-1313
</td>
<td style="text-align:center;">
KentProbst
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
68819
</td>
<td style="text-align:center;">
16-1332
</td>
<td style="text-align:center;">
RobRaffety
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
60297
</td>
<td style="text-align:center;">
16-1614
</td>
<td style="text-align:center;">
KristiFrazier
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
82018
</td>
<td style="text-align:center;">
16-1626
</td>
<td style="text-align:center;">
RubenRamirez
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
69187
</td>
<td style="text-align:center;">
16-1896
</td>
<td style="text-align:center;">
DanaleeBuhler
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
82143
</td>
<td style="text-align:center;">
16-1963
</td>
<td style="text-align:center;">
MollyMollotova
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
18367
</td>
<td style="text-align:center;">
16-1965
</td>
<td style="text-align:center;">
CarlishaBell
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
103181
</td>
<td style="text-align:center;">
18-3008
</td>
<td style="text-align:center;">
JackieLang
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
53751
</td>
<td style="text-align:center;">
13-1084
</td>
<td style="text-align:center;">
ElaineTise
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition, Member
</td>
</tr>
<tr>
<td style="text-align:left;">
53067
</td>
<td style="text-align:center;">
13-0852
</td>
<td style="text-align:center;">
StanleyLombardo
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition, Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
91678
</td>
<td style="text-align:center;">
17-1025
</td>
<td style="text-align:center;">
CandraBohjalian
</td>
<td style="text-align:center;">
Lone Star Badge,Conference Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
95504
</td>
<td style="text-align:center;">
18-0762
</td>
<td style="text-align:center;">
SheriChandler
</td>
<td style="text-align:center;">
Lone Star Badge,Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
72928
</td>
<td style="text-align:center;">
16-0097
</td>
<td style="text-align:center;">
DruSellers
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
72934
</td>
<td style="text-align:center;">
16-0126
</td>
<td style="text-align:center;">
Terrence CMason
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
74629
</td>
<td style="text-align:center;">
16-0713
</td>
<td style="text-align:center;">
DavidDorsey
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
75606
</td>
<td style="text-align:center;">
16-0735
</td>
<td style="text-align:center;">
Miwa DeSilva
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
71188
</td>
<td style="text-align:center;">
16-0875
</td>
<td style="text-align:center;">
RoseanneFurst
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
77934
</td>
<td style="text-align:center;">
16-0937
</td>
<td style="text-align:center;">
KellieOlive
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
67430
</td>
<td style="text-align:center;">
16-1031
</td>
<td style="text-align:center;">
AndreaDelott
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
79062
</td>
<td style="text-align:center;">
16-1060
</td>
<td style="text-align:center;">
GretchenCion
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
81100
</td>
<td style="text-align:center;">
16-1065
</td>
<td style="text-align:center;">
ElizabethSchaaf
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
76106
</td>
<td style="text-align:center;">
16-1068
</td>
<td style="text-align:center;">
AllisonHerrera
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
63368
</td>
<td style="text-align:center;">
16-1075
</td>
<td style="text-align:center;">
RachellMartinez
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
30907
</td>
<td style="text-align:center;">
16-1095
</td>
<td style="text-align:center;">
JayMatthew
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
74921
</td>
<td style="text-align:center;">
16-1122
</td>
<td style="text-align:center;">
AndrewGray
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
81394
</td>
<td style="text-align:center;">
16-1205
</td>
<td style="text-align:center;">
MalcolmWeekes
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
53868
</td>
<td style="text-align:center;">
17-0337
</td>
<td style="text-align:center;">
KathleenMcDonald
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
62187
</td>
<td style="text-align:center;">
17-0443
</td>
<td style="text-align:center;">
ElisabethMisner
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
91249
</td>
<td style="text-align:center;">
17-0571
</td>
<td style="text-align:center;">
AmySirizi
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
91408
</td>
<td style="text-align:center;">
17-0594
</td>
<td style="text-align:center;">
RichardWarke
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
72648
</td>
<td style="text-align:center;">
17-0768
</td>
<td style="text-align:center;">
AniJarrett
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
91526
</td>
<td style="text-align:center;">
17-0934
</td>
<td style="text-align:center;">
RyanBarlow
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
70387
</td>
<td style="text-align:center;">
17-1031
</td>
<td style="text-align:center;">
MichaelStreiter
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
68150
</td>
<td style="text-align:center;">
17-1048
</td>
<td style="text-align:center;">
JacklynThrapp
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
91683
</td>
<td style="text-align:center;">
17-1063
</td>
<td style="text-align:center;">
MarciaGleeson
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
92187
</td>
<td style="text-align:center;">
17-1451
</td>
<td style="text-align:center;">
DennisMonn
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
90178
</td>
<td style="text-align:center;">
17-1749
</td>
<td style="text-align:center;">
SaikatMukherjee
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
92519
</td>
<td style="text-align:center;">
17-1772
</td>
<td style="text-align:center;">
BillThomas
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
19869
</td>
<td style="text-align:center;">
17-1848
</td>
<td style="text-align:center;">
DanielKorb
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
92582
</td>
<td style="text-align:center;">
17-1896
</td>
<td style="text-align:center;">
ScottieRitter
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
84626
</td>
<td style="text-align:center;">
17-1967
</td>
<td style="text-align:center;">
RayUzwyshyn
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
29171
</td>
<td style="text-align:center;">
17-2066
</td>
<td style="text-align:center;">
GarrettGraham
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
92612
</td>
<td style="text-align:center;">
17-2067
</td>
<td style="text-align:center;">
AsadDurrani
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
96780
</td>
<td style="text-align:center;">
18-1104
</td>
<td style="text-align:center;">
GiaLomenzo
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
100149
</td>
<td style="text-align:center;">
18-1422
</td>
<td style="text-align:center;">
AdrienneHarmon
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
100713
</td>
<td style="text-align:center;">
18-1430
</td>
<td style="text-align:center;">
JessicaFerrato
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
101783
</td>
<td style="text-align:center;">
18-1442
</td>
<td style="text-align:center;">
MariaRublein
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
759
</td>
<td style="text-align:center;">
18-1446
</td>
<td style="text-align:center;">
JayBoisseau
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
102111
</td>
<td style="text-align:center;">
18-1513
</td>
<td style="text-align:center;">
RobertReichle
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
35945
</td>
<td style="text-align:center;">
18-1583
</td>
<td style="text-align:center;">
DustinHennessey
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
98153
</td>
<td style="text-align:center;">
18-1596
</td>
<td style="text-align:center;">
AaronMay
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
81095
</td>
<td style="text-align:center;">
18-1811
</td>
<td style="text-align:center;">
MaryTrouba
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
103026
</td>
<td style="text-align:center;">
18-2257
</td>
<td style="text-align:center;">
ScottDobbie
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
102964
</td>
<td style="text-align:center;">
18-2267
</td>
<td style="text-align:center;">
Laura MesserJackson
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
103039
</td>
<td style="text-align:center;">
18-2276
</td>
<td style="text-align:center;">
BrianSchwab
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
103044
</td>
<td style="text-align:center;">
18-2289
</td>
<td style="text-align:center;">
DanielDerksen
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
103050
</td>
<td style="text-align:center;">
18-2306
</td>
<td style="text-align:center;">
Kimi BuserClancy
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
95912
</td>
<td style="text-align:center;">
18-2349
</td>
<td style="text-align:center;">
RobertTartell
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
82162
</td>
<td style="text-align:center;">
16-2021
</td>
<td style="text-align:center;">
LeighLewis
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
100413
</td>
<td style="text-align:center;">
18-1451
</td>
<td style="text-align:center;">
STANLEYBARTON
</td>
<td style="text-align:center;">
Stickers - Ask me About My Script,,Lone Star Badge,Pitch Competition
</td>
</tr>
<tr>
<td style="text-align:left;">
101757
</td>
<td style="text-align:center;">
18-1370
</td>
<td style="text-align:center;">
Leslie SimmonsSimmons
</td>
<td style="text-align:center;">
Stickers - Ask me About My Script,Lone Star Badge,Pitch Competition
</td>
</tr>
</tbody>
</table>
#### People who bought Twice

    which(df$counts == 2)

    ## [1] 17 24 35 39 79

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
MainID
</th>
<th style="text-align:center;">
OrderNo
</th>
<th style="text-align:center;">
FullName
</th>
<th style="text-align:center;">
Description
</th>
<th style="text-align:center;">
X
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
34442
</td>
<td style="text-align:center;">
17-1165
</td>
<td style="text-align:center;">
CharlotteShafer
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
10613
</td>
<td style="text-align:center;">
14-1028
</td>
<td style="text-align:center;">
DavidWheeler
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
47501
</td>
<td style="text-align:center;">
14-0400
</td>
<td style="text-align:center;">
GloriaBankler
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
53774
</td>
<td style="text-align:center;">
17-0955
</td>
<td style="text-align:center;">
HelenLemm
</td>
<td style="text-align:center;">
Film Pass,Film Pass,Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
37096
</td>
<td style="text-align:center;">
18-0305
</td>
<td style="text-align:center;">
RachelWilkins Patel
</td>
<td style="text-align:center;">
Lone Star Badge,Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
NA
</td>
</tr>
</tbody>
</table>
#### People who bought Three Times

    which(df$counts == 3)

    ## [1] 18

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
MainID
</th>
<th style="text-align:center;">
OrderNo
</th>
<th style="text-align:center;">
FullName
</th>
<th style="text-align:center;">
Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
49126
</td>
<td style="text-align:center;">
14-0949
</td>
<td style="text-align:center;">
CindyWeigand
</td>
<td style="text-align:center;">
Film & Food 23 Oct (Wed) 7:00pm , Lone Star Badge, Pitch Competition
</td>
</tr>
</tbody>
</table>
