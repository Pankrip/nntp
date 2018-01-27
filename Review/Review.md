# Recenzja projektu *"Statystyki pliku tekstowego"*

Repozytorium GitHub: ["text-stats"](https://github.com/logx/text-stats)

Recenzowany przez nas projekt charakteryzuje się logicznym i dobrze 
przemyślanym podziałem na moduły oraz odpowiadajace
im pliki i foldery - każdy moduł zawiera pewną grupę podobnych do siebie 
pod względem funkcjonalności elementów, które
razem są odpowiedzialne za pewien dobrze ustalony zakres odpowiedzialności 
całego systemu. Niestety nie do końca zadbano
o przejrzystość samego repozytorium - plik `README.md` zawiera jedynie 
informacje o autorach, w żaden sposób nie informuje
użytkownika o sposobie instalacji programu ani o jego użytkowaniu. Również 
plik `package.yaml` nie został do końca
uzupełniony przydatnymi informacjami, takimi jak np. link do repozytorium. 

W kwestii testów również mamy mieszane uczucia. Zacznijmy od testów 
jednostkowych, które w tym przypadku zostały napisane przy pomocy pakietu 
`HUnit`: jest ich całkiem sporo, jednakże nie testują one potecjalnie 
najgroniźniejszych dla poprawnej funkcjonalności systemu przypadków 
granicznych (tzw. corner cases), a jedynie przypadki najczęstsze. Ponadto dla 
każdej funkcji napisano zaledwie jeden test, co w przypadku pozytywnego 
przejścia całego test-suit'u raczej nie pozwoli na posiadanie stuprocentowej 
pewności o poprawnej implementacji. Testy również nie zostały w żaden 
sposób opisane przy pomocy komentarzy, co może w przyszłości spowodować, 
że nowe osoby,  którym przekazano by pieczę nad projektem, mogłby by mieć 
nieco pod górkę. Nic nie możemy powiedzieć natomiast na temat testów z 
wykorzystaniem pakietu `QuickCheck`, z uwagi, że w całym projekcie znajduje 
się zaledwie jeden test, który w dodatku dotyczy funkcji pochodzącej z 
podstawowego pakietu. 

Przejdźmy teraz do samego kodu zawartego w folderze `src`. Posiada on 
zwięzłą i dobrze napisaną dokumentację w duchu systemu `Haddock`. Cechuje 
się również jednolitą strukturą  w każdym pliku oraz czytelnymi 
wcięciami i ogólnie dość schludnym formatowaniem. Jednakże użycie 
długiego formatu komentarza dokumentującego funkcję, czyniąc nieraz opis 
funkcji dłuższym od jej samej (np. 4 linijkowy komentarz do 2-linijkowej 
funckji) może w opini recenzentów "przytłoczyć" czytelnika kodu, 
zmniejszając jego czytelność, osiągając tym cel przeciwny do 
zamierzonego. Zadbano o nienadużywanie nawiasów, stosowano je jedynie w 
miejscach obsolutnie koniecznych, a tam gdzie się dało, stosowano 
zdecydowanie bardziej czytelne rozwiązanie w postaci operatorów `.` oraz `$`. 
Warto również zwrócić uwagę, że kod nie jest w żaden sposób 
"przekombinowany", tj. wszystkie funkcję zostały napisane w sposób, który 
dla doświadczonego programisty Haskella będzie od razu czytelny i łatwy do 
zrozumienia. Drobnym minusem jest z kolei dość macosze podejście do obsługi 
błędów oraz całkowity brak wykorzystania bardziej rozbudowanych konstrukcji 
językowych, takich jak funktory i monady.
W opini recenzentów użycie funkcji `try` zamiast `catch`/`handle` w przypadku 
obsługi błędu, skutkuje niepotrzebnym użyciem typu monadycznego `Either`.
Wracając do dokumentacji funkcji raz jeszcze, poza drobnymi błędami 
powstałymi przy najprawdopodbniej mechanicznej replikacji komentarzy (np. 
`takes one arguments`), to stosowanie komentarza typu `it takes x arugments, of 
type 'a' nd 'b' and returns a 'c'` jest całkowicie zbędne, mając na uwadzę 
to że wszelkich informacji zawartych w tej lini dokumentacji można 
dowiedzieć się z sygnatury typu która została podana przy każdej funkcji 
(co jest plusem).

Importy zależności zostały dokonane za pomocą niekwalifikowanych importów, 
co może skutkować konfliktem nazw identyfikatorów z różnych modułów, 
przy czym konflikt ten z uwagi na leniwą naturą języka Haskell może zostać 
niezauważony aż do użycia winnej funkcji, a przepisanie projektu np. na 
używanie tylko importów kwalifikowanych po ujawnieniu konfliktu może być 
czasochłonne.

Odnośnie stylu kodowania, autorom recenzji nie przypadło do gustu stosowanie 
stylu _point-full_ na siłę (np. `(\x -> removePunctuation x)` zamiast 
równoważnego `removePunctuation`) oraz [stosowanie notacji `do`][1]. Naszym 
zdaniem użycie `do` ukrywa kompozycyjną naturę języka, oraz daje złudne 
uczucie sekwencyjnej natury wykonania akcji, co nie jest prawdą, jak zresztą 
przekonali się o tym na własej skórze recenzenci projektu.

--- 
##### uwagi odnośnie poszczególnych funkcji
`countWords (MkDocument path content) = length [x | x <- words content]` </br>
_List comprehension_ w powyższym przykładzie jest całkowicie zbędne 
(równoważnym jest użycie `length $ words content`)

`countWordOcc (MkDocument path content) word = length [x | x <- words content, 
x == word]`
Użycie _list comprehension_ w tym kontekście można zastąpić użyciem 
funkcji `filter` wraz z funkcją zdefiniowaną przez twórców w module 
`Utils`, lub, używając bardziej funkcyjnego stylu, przy użyciu _fold_

w funkcji `countLettersOcc` stałe znakowe ASCII zostały zakodowane przy 
użyciu liczb dziesiętnych, co zmniejsza przejrzystość kodu (proponowane 
rozwiązanie: `fromEnum 'c'`)

``countFreqWords (MkDocument path content) = take 10 $ sortBy (flip compare) $ 
filter (\(i, `str) -> i > 1) $ wordOccurrence content`` </br>
przypadkowe wprowadzenie znaku `` ` `` do deklaracji powoduje błąd kompilacji

[1]: https://wiki.haskell.org/Do_notation_considered_harmful

