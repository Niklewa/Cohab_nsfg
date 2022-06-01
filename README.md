# Cohab_nsfg

You can find an english version at the bottom of this page.  
Polish:   

W plikach w formacie R przeprowadzam analizę wpływu czynników: kohabitacji przedmałżeńskiej, bycia wierzącym oraz 
bycia dewotą religijnym na prawdopodobieństwo rozwodu. Czynię to na podstawie bazy danych NSFG z lat 2002 - 2019, 
na populacji ponad 70 000 osób.  

Udało mi się wykazać iż przekonanie, że to kohabitacja przedmałżeńska bezpośrednio wpływa na możliwość rozwodu jest fałszywe.  

Istnieją mocniejsze korelaty niż kohabitacja, jest nim np. bycie ateistą. Gdzie dodatkowo można wysnuć wniosek iż bycie ateistą
jest ukrytym korelatem, albowiem silnie wpływa na prawdopodobieństwo kohabitacji przedmałżeńskiej.   

Plik ipynb odpowiada za pobranie plików ze strony źródłowej oraz ich zapis w formacie .csv. Kolejne dwa pliki w R stanowią główną część pracy. 
Wybrane pliki .csv zostały ujednolicone do formatu małych liter, 
gdyż jako jedyne były zapisane wielkimi, zostało to przeze mnie zrobione w excelu. Oprócz tego dodałem dwa pliki z lat
2006-2010, gdyż ich wersja pobrana ze strony głównej była nieodpowiednia / uszkodzona. Jest to wspomniane w pliku pythonowym.  

Do samej analizy wystarczający jest plik dat_main.csv.    

Źródło danych: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/stata/   
Dokumentacja danych: https://www.cdc.gov/nchs/nsfg/nsfg_cycle6.htm   

English:   

In this project, using R, I conduct an analysis of probability of divorce given following factors: 
cohabitation before marriage, being an atheist, being a religious devot. For that, I use NSFG database from years 2002 - 2019, 
the total number of participants is 70 000 thousands.

I was able to falsify the belief, that cohabitating before marriage is a direct factor that increases the probability of divorce.

There are far more stronger correlates then cohabitation, e.g. being an atheist. Also, the following conclusion can be drown, that being an atheist
is a hidden correlate, because it strongly affects the probability of cohabitating before marriage.

In ipynb file I just downloaded the files and save them in the .csv format. The two R files are the main part of my work. Some automatically dowloaded files
had to be unified to the rest in excell, there was a difference of capital and small letters. Apart from that, I had to add a pair (2006-2010) of iles from
a different source (in different format) because their source version was not compatible / corrupted. I mentioned that in the python file.

For the purpose of the sole analysis, done in the second R file, dat_main.csv file is sufficient.

Source of data: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/stata/  
Data documentation: https://www.cdc.gov/nchs/nsfg/nsfg_cycle6.htm 

