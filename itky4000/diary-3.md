# ITKY4000, Oppimispäiväkirja 3

- Mikael Myyrä (mikael.b.myyra@jyu.fi), päätösanalytiikan ja
  teknis-matemaattisen mallintamisen opintosuunta

## HOPSistani

Tavoitteeni maisteriopinnoissa on yksinkertaisesti käyttää mahdollisimman paljon
aikaa niiden asioiden opiskeluun, jotka minua oikeasti kiinnostavat. Näitä
asioita ovat erityisesti fysiikkasimulaatiot ja tietokonegrafiikka sekä niiden
taustalla toimiva matematiikka. Yliopistossa haluan keskittyä sellaisiin
asioihin, joita en vielä osaa kovin hyvin ja joissa henkilökohtaisesta
opetuksesta on hyötyä, ja hallitsen jo ohjelmoinnin erittäin hyvin, joten
painotan HOPSissani puhdasta matematiikkaa.

Alkuun tarvitsen hieman täydennystä matematiikan aineopinnoista, että ymmärrän
fysiikan osittaisdifferentiaaliyhtälöitä ja grafiikan lineaarialgebraa. Tätä
varten olen valikoinut mukaan vektoricalculusta ja matriisilaskentaa. Sitten
alkaa pääaineen puolelta teknis-matemaattisen mallintamisen opintosuunta, jonka
sisältö on pitkälti suoraan juuri sitä, mitä haluankin opiskella.

Valinnaisissa syventävissä opinnoissa minulla on lähinnä jo kandidaatin
tutkinnon aikana tehtyjä kursseja tietokonegrafiikkaan, peleihin ja
ohjelmointiin liittyen. Ne ovat silloisen kiinnostuksen mukaan valittuja eivätkä
vastaa suoraan tämän maisterin tutkinnon tavoitteisiin, mutta joukossa on
tietokonegrafiikkaa ja fysiikkaakin. Uutena tulee näillä näkymin matematiikan
laitoksen osittaisdifferentiaaliyhtälöiden kurssi, joka ei tosin ole vielä aivan
lukkoon lyöty valinta. Tietotekniikan puolellakin nimittäin opiskellaan ODYjä.

Projektikurssi on minun elämäntilanteessani hankala juttu, koska jokainen niistä
vaatii enemmän viikoittaista aikaa, kuin minulla jää töistä. Kurssi leikkaisi
siis väistämättä vapaa-aikaani. Olen kuullut huhua, että työkokemus on
mahdollinen tapa korvata sovellusprojekti, mikä olisi minulle paras vaihtoehto,
mutta tätä on vielä selviteltävä. Jos se ei onnistu, vaihtoehdot ovat
peliprojekti huviksi ja pelikehitysharrastukseni tueksi tai tutkimusprojekti
uuden oppimisen maksimoinniksi.

## Gradusta

Valitsin luettavakseni Juuso Tenhusen gradun Kelluvuuden mallintaminen
videopeleissä (2019), koska se vaikutti otsikon perusteella olevan lähellä omaa
alaani. Kelluvuus liittyy läheisesti oman kandidaatintutkielmani aiheeseen,
nestesimulaatioihin.

### Sisältö

Tenhunen tutkii gradussaan kahta reaaliaikaista menetelmää kovaan kappaleen
kokemien nostevoimien approksimointiin vedessä. Vettä ei simuloida, vaan sen
pinta luodaan kohinafunktion avulla.

Ensimmäisenä Tenhunen tarkastelee pelkistettyä mallia, jossa kappaleen
keskipistettä liikutetaan kohti veden pintaa rajoitetulla nopeudella ja asentoa
käännetään pinnan suuntaiseksi. Tämä malli on hyvin yksinkertainen, mutta
fysikaalisesti perusteeton.

Toinen tarkasteltava menetelmä perustuu fysikaalisiin malleihin. Kappaleen
syrjäyttämää veden tilavuutta approksimoidaan nosteen laskemiseksi, ja
vastusvoimina mallinnetaan ilmanvastusta, viskoottista liukumakitkaa,
painevastusta ja pinnan iskuvoimia.

Tenhunen soveltaa mallia kolmioverkkoina rakennettuihin kappaleisiin, joiden
leikkauksia vedenpinnan kanssa tarvitaan nosteen arviointiin. Leikkaus
toteutetaan lineaarisena approksimaationa, ja sen tarkkuus riippuu kolmioverkon
tiheydestä.

Tenhunen mittasi edellä mainittujen menetelmien vaatimaa laskenta-aikaa.
Jälkimmäisessä hän testasi lisäksi kahta eri algoritmia vedenalaisten
tilavuuksien ja massakeskipisteiden laskentaan. Odotetusti monimutkaisempi
fysikaalinen malli osoittautui raskaammaksi laskettavaksi. Erityisesti
kolmioverkkojen ja vedenpinnan leikkauksiin kului paljon aikaa, jos verkot
olivat suuria.

### Arviointia

Pääosa gradun tekstisisällöstä keskittyy toteutusyksityiskohtien kuvailuun
Unity-pelimoottorissa ja C#-kielessä. Koska gradun kohdeyleisö on tietotekniikan
maisteriopiskelijat, voidaan olettaa, että lukijat osaavat ohjelmoida, ja näin
kattava toteutuksen kuvailu on siten tarpeetonta. Tutkittavana aiheena on
kelluntamallien tehokkuuden ja ominaisuuksien vertailu, mutta itse malleille
annetaan hyvin vähän palstatilaa ja niiden ominaisuuksia ei analysoida
matemaattisesti lainkaan. Teorian käsittely on referaattimaista ja suppeaa.

Myös kieliasu on huolimaton; yhdyssanavirheitä ja puhekielimäisiä ilmauksia
esiintyy jatkuvasti. Rakenteeltaan gradu on kuitenkin onnistunut ja hyvin
sidosteinen. Etenemisjärjestys on looginen ja asioiden välille luodaan
yhteyksiä.

Gradun lähdemateriaali koostuu lähinnä pelialan blogi- ja artikkelijulkaisuista.
Näiden perusteella molempia tarkasteltavia menetelmiä on jossain muodossa jo
käytetty pelialan tuotteissa. Siispä ne on todettu reaaliajassa
toteutuskelpoisiksi. On myös ilmiselvää, että monimutkaisempi malli vaatii
enemmän laskentaa ja on siten hitaampi, joten myöskään laskentanopeuksien
vertailusta ei saada kiinnostavaa tietoa. Mielestäni tämä gradu ei näin ollen
tuota mitään uutta tietoa.

### Fiiliksiä

Tämä gradu osoittautui huonoksi valinnaksi tähän tehtävään. Otsikko herätti
kiinnostuksen, ja sainkin irti muutaman rivin verran ihan mielenkiintoista
tietoa kellumisen mallintamisesta, mutta mallien kuvailu tekstissä oli todella
suppeaa ja ylivoimainen enemmistö tekstistä täysin turhaa koodin selostamista.
Suuri osa koodista oli vieläpä Unityn kirjastojen käyttöä, jota kuvailtiin
suorastaan tuskallisen yksityiskohtaisesti. Kaiken tämän voisi toteuttaa millä
tahansa ohjelmointikielellä ja kirjastolla, eikä tämä yksittäinen toteutus ole
millään tapaa opettavainen tai kiinnostava.

Valitsemani gradun sisällöstä huolimatta oli toki hyödyllistä nähdä, miltä
valmis gradu suunnilleen näyttää rakenteen ja arviointikriteerien puolesta. Tämä
auttaa vähän varautumaan siihen, minkälainen työmäärä on kyseessä.

Oli mielenkiintoista huomata viime vuosina julkaistujen gradujen listasta, miten
vähän tiedekunnassamme on matematiikasta ja grafiikasta kiinnostuneita. Huomasin
valitsemani gradun lisäksi vain yhden suoraan aiheeseen liittyvän työn
planeettojen renderöinnistä. Esimerkkejä juuri sellaisesta työstä, mitä itse
haluan tehdä, on siis hieman vaikea löytää, mutta tämä ei minua pelota vaan
pikemminkin innostaa.
