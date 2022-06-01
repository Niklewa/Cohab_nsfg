# Cohab_nsfg

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
