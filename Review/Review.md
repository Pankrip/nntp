# Recenzja projektu *"Statystyki pliku tekstowego"*

Repozytorium GitHub: ["text-stats"](https://github.com/logx/text-stats)

Recenzowany przez nas projekt charakteryzuje się logicznym i dobrze przemyślanym podziałem na moduły oraz odpowiadajace
im pliki i foldery - każdy moduł zawiera pewną grupę podobnych do siebie pod względem funkcjonalności elementów, które
razem są odpowiedzialne za pewien dobrze ustalony zakres odpowiedzialności całego systemu. Niestety nie do końca zadbano
o przejrzystość samego repozytorium - plik `README.md` zawiera jedynie informacje o autorach, w żaden sposób nie informuje
użytkownika o sposobie instalacji programu ani o jego użytkowaniu. Również plik `package.yaml` nie został do końca
uzupełniony przydatnymi informacjami, takimi jak np. link do repozytorium. 

W kwestii testów również mamy mieszane uczucia. Zacznijmy od testów jednostkowych, które w tym przypadku zostały napisane przy pomocy pakietu `HUnit`: jest ich całkiem sporo, jednakże nie testują one potecjalnie najgroniźniejszych dla poprawnej funkcjonalności systemu przypadków granicznych (tzw. corner cases), a jedynie przypadki najczęstsze. Ponadto dla każdej funkcji napisano zaledwie jeden test, co w przypadku pozytywnego przejścia całego test-suit'u raczej nie pozwoli na posiadanie stuprocentowej pewności o poprawnej implementacji. Testy również nie zostały w żaden sposób opisane przy pomocy komentarzy, co może w przyszłości spowodować, że nowe osobe,  którym przekazano by pieczę nad tym projektem, mogłby by mieć nieco pod górkę. Nic nie możemy powiedzieć natomiast na temat testów z wykorzystanie pakietu `QuickCheck`, z uwagi, że w całym projekcie znajduje się zaledwie się zaledwie jeden test, który w dodatku dotyczy funkcji pochodzącej z podstawowego pakietu. 

Przejdźmy teraz do samego kodu zawartego w folderze `src`. 
