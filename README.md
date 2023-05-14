## Mikołaj Szkaradek

##### Język:

Szkarson to statycznie typowany język będący połączeniem pewnych fragmentów Rusta i C++.

## Opis Rozwiązania

##### Uruchamianie programu:

Program buduje polecenie make, tworzy ono plik wykonywalny interpreter. Aby uruchomić program należy wpisać ./interpreter file_name
lub
./interpreter                  i wczytać cały program ze standardowego wejścia. 

##### Opis rozwiązania:

Rozwiązanie podzielone jest na kilka plików. W folderze Interpreter, są pliki Program.hs oraz StatementsAndExpressions.hs, w których dostępna jest implementacja interpretera. W folderze Typechecker, są pliki Program.hs, StatementsAndExpressions.hs oraz ErrorMessages.hs, w tych plikach dostępna jest implementacja TypeCheckera. W rozwiązaniu są również pliki Types.hs, Utils.hs oraz Main.hs. W pliku Utils.hs dostępne są wspólne dla interpretera i typecheckera funkcje pomocnicze. W pliku Types.hs zdefiniowane są wszystkie typy, z których korzystam w rozwiązaniu. 

W przypadku interpretera istotne są typy:

```haskell
data Value = VInt Integer | VBool Bool | VString String | VVoid
type Func = ([Arg], Block, Env, Type)

data Env = Env
  { _varEnv :: Map Ident Loc,
    _funcEnv :: Map Ident Func
  }

data Store = Store
  { _store :: Map Loc Value,
    _next :: Loc
  }

type IM a = ExceptT String (StateT Store (ReaderT Env IO)) a
```

Funkcje przechowywane są w środowisku, jako krotka ``type Func = ([Arg], Block, Env, Type)``, w ten sposób zapamiętujemy z jakimi argumentami powinna być wykonana funkcje, pamiętamy ciało funkcji (Block), środowisko z momentu deklaracji funkcji, oraz typ funkcji. (Jeśli funkcja nie ma returna ale jest konkretnego typu, to zwraca domyślną wartość tego typu). Wołając funkcję należy zaktualizować środowisko z czasu deklaracji o argumenty funkcji oraz o samą funkcję aby zapewnić rekurencję.

Interpreter korzysta z monady `ExceptT String (StateT Store (ReaderT Env IO)) a` 
State przechowuje globalny stan programu, Reader przechowuje środowisko, w którym obecnie wykonujemy instrukcje, a Except pozwala na rzucanie błędów czasu wykonania.

Interpreter uruchamiany jest za pomocą funkcji interpret z pliku Interpreter/Program.hs

W przypadku Typecheckera istotne są typy:

```haskell
data TType = IntT | BoolT | StrT | VoidT | RefT TType

data TypeEnv = TypeEnv
  { _typeVarEnv :: Map Ident TType,
    _typeFuncEnv :: Map Ident ([Arg], TType),
    _in_func :: TType 
  }

type TM a = ExceptT String (ReaderT TypeEnv IO) a
```

W środowisku trzymamy informację o typach wszystkich obecnie widocznych zmiennych i funkcji oraz o typie funkcji, w której obecnie jest sterowanie programu. W przypadku Typecheckera nie potrzebna jest monada state, ponieważ istotne są tylko typy zmiennych w obecnym środowisku. 

Typechecker uruchamiany jest za pomocą funkcji  typecheck z pliku TypeChecker/Program.hs

##### Program:

Program jest listą definicji funkcji oraz zmiennych globalnych. W programie musi być funkcja `main`. 

##### Typy:

Dostępne są 4 typy: `int`, `bool`, `string`, `void`. Literały liczbowe `int` to liczby całkowite `/([0-9])+`. Literały napisowe zapisujemy w cudzysłowach. Typ `bool` przyjmuje wartości `true` lub `false`.  Typ `void` służy do przekazania funkcji informacji, że ma nic nie zwracać.

##### Operatory arytmetyczne i porównania:

Dostępne są operatory arytmetyczne: (`+,-,*,/,^`) oraz operatory porównawcze: (`==, !=, >, <, >=, <=`). Ich działanie jest standardowe jak w typowych językach programowania. Operatory arytmetyczne zwracają typ `int`, a porównania typ `bool`.

##### Zmienne i przypisania:

Możliwe jest tworzenie zmiennych o identyfikatorach takich jak w typowych językach, oraz przypisywanie do zmiennych wartości. Można przypisać wartość do zmiennej każdego typu, poza typem `void`. 

##### Instrukcje print, while, if:

Instrukcje te są klasyczne jak w typowych językach programowania, dokładna składnia dostępna jest w gramatyce.

##### Funkcje:

W języku można definiować zagnieżdżone funkcje oraz korzystać z rekurencji.

##### Przekazywanie argumentów:

Argumenty można przekazywać przez wartość lub zmienną.

Poniżej dostępna jest tabelka z cechami języka, które zamierzam zrealizować.

##### Tablice wielowymiarowe:

W języku miałyby być dostępne są tablice wielowymiarowe `Type []...[] ident`.  Można by było odwołać się do zmiennej jak w C++: `arr[i][j][k]`. Niestety nie udało mi się zrealizować tej funkcjonalności w pierwszym terminie.

##### Krotki:

Miał być dostępny jest typ `tuple <[Type]>` który pozwalałby tworzyć dowolnie długie i zagnieżdżone krotki.Można by było również dokonać przypisania <int a, bool b, string c> = <|1,true,"abc"|> które działałoby intuicyjnie.
Niestety nie udało mi się zrealizować tej funkcjonalności w pierwszym terminie.

##### Wbudowane funkcje:

W Szkarsonie dostępne są różne wbudowane funkcje do operacji na stringach.

`charAt(Expr)`, `reverse()` , `length()`, `append(String)` , `substr(Expr1,Expr2)`, `setCharAt(Expr)` 

##### Zaktualizowana tabelka cech

Plusem oznaczone są zrealizowane funkcjonalności.

```
  Na 15 punktów
  + 01 (trzy typy)
  + 02 (literały, arytmetyka, porównania)
  + 03 (zmienne, przypisanie)
  + 04 (print)
  + 05 (while, if)
  + 06 (funkcje lub procedury, rekurencja)
  + 07 (przez zmienną / przez wartość / in/out)
  Na 20 punktów
  + 09 (przesłanianie i statyczne wiązanie)
  + 10 (obsługa błędów wykonania)
  + 11 (funkcje zwracające wartość)
  Na 30 punktów
  + 12 (4) (statyczne typowanie)
  + 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  - 14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
  - 15 (2) (krotki z przypisaniem)
  - 16 (1) (break, continue)
```

