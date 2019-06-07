Some edits I did for the original excel file: 2013-2018 Pitch Orders:

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
DateReceived
</th>
<th style="text-align:center;">
FirstName
</th>
<th style="text-align:center;">
LastName
</th>
<th style="text-align:center;">
FullName
</th>
<th style="text-align:center;">
Description
</th>
<th style="text-align:center;">
ord\_OIT\_\_OrderItems..Price
</th>
<th style="text-align:right;">
ord\_OIT\_\_OrderItems..PriceBasisCode
</th>
<th style="text-align:left;">
ord\_OIT\_\_OrderItems..ProgEventCode
</th>
<th style="text-align:center;">
ord\_MDB\_\_MainDBviaCustID..Address1
</th>
<th style="text-align:center;">
ord\_MDB\_\_MainDBviaCustID..City1
</th>
<th style="text-align:center;">
ord\_MDB\_\_MainDBviaCustID..State1
</th>
<th style="text-align:center;">
ord\_MDB\_\_MainDBviaCustID..Postalcode1
</th>
<th style="text-align:center;">
EMail
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
05/17/2017
</td>
<td style="text-align:center;">
Charlotte
</td>
<td style="text-align:center;">
Shafer
</td>
<td style="text-align:center;">
CharlotteShafer
</td>
<td style="text-align:center;">
Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
125,20
</td>
<td style="text-align:right;">
UNSP,FULL
</td>
<td style="text-align:left;">
202,301
</td>
<td style="text-align:center;">
12548 Sir Christophers Cv
</td>
<td style="text-align:center;">
Austin
</td>
<td style="text-align:center;">
TX
</td>
<td style="text-align:center;">
78729
</td>
<td style="text-align:center;">
<CharWeb@grandecom.net>
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
08/24/2014
</td>
<td style="text-align:center;">
David
</td>
<td style="text-align:center;">
Wheeler
</td>
<td style="text-align:center;">
DavidWheeler
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
<td style="text-align:center;">
125, 20
</td>
<td style="text-align:right;">
FULL, FULL
</td>
<td style="text-align:left;">
202, 301
</td>
<td style="text-align:center;">
10 Bowdoin St \#305
</td>
<td style="text-align:center;">
Boston
</td>
<td style="text-align:center;">
MA
</td>
<td style="text-align:center;">
2114
</td>
<td style="text-align:center;">
<davidrwheeler@comcast.net>
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
03/03/2014
</td>
<td style="text-align:center;">
Gloria
</td>
<td style="text-align:center;">
Bankler
</td>
<td style="text-align:center;">
GloriaBankler
</td>
<td style="text-align:center;">
Lone Star Badge, Pitch Competition
</td>
<td style="text-align:center;">
125, 20
</td>
<td style="text-align:right;">
FULL, FULL
</td>
<td style="text-align:left;">
202, 301
</td>
<td style="text-align:center;">
5511 W Beach Circle
</td>
<td style="text-align:center;">
Austin
</td>
<td style="text-align:center;">
TX
</td>
<td style="text-align:center;">
78734
</td>
<td style="text-align:center;">
<grbankler@gmail.com>
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
04/28/2017
</td>
<td style="text-align:center;">
Helen
</td>
<td style="text-align:center;">
Lemm
</td>
<td style="text-align:center;">
HelenLemm
</td>
<td style="text-align:center;">
Film Pass,Film Pass,Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
50,75,99,20
</td>
<td style="text-align:right;">
ERLD,FULL,UNSP,FULL
</td>
<td style="text-align:left;">
201,201,202,301
</td>
<td style="text-align:center;">
1809 Frazier Ave
</td>
<td style="text-align:center;">
Austin
</td>
<td style="text-align:center;">
TX
</td>
<td style="text-align:center;">
78704
</td>
<td style="text-align:center;">
<helenlemm@me.com>
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
12/07/2017
</td>
<td style="text-align:center;">
Rachel
</td>
<td style="text-align:center;">
Wilkins Patel
</td>
<td style="text-align:center;">
RachelWilkins Patel
</td>
<td style="text-align:center;">
Lone Star Badge,Lone Star Badge,Pitch Competition
</td>
<td style="text-align:center;">
100,100,20
</td>
<td style="text-align:right;">
ERLD,ERLD,ERLD
</td>
<td style="text-align:left;">
202,202,301
</td>
<td style="text-align:center;">
3608 Palmerston Rd
</td>
<td style="text-align:center;">
Shaker Heights
</td>
<td style="text-align:center;">
OH
</td>
<td style="text-align:center;">
44122
</td>
<td style="text-align:center;">
<rachel@rachelwilkins.com>
</td>
</tr>
</tbody>
</table>
