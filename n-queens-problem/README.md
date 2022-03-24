# ФерSat

### Запуск
Перед запуском рекомендуется запустить `cabal update`

Запуск делается командой `cabal run 'n-queens-problem' 4`

### Пример использования

```
$ cabal run 'n-queens-problem' 4
----------
Solution:
----------
[..Q.]
[Q...]
[...Q]
[.Q..]
----------
$ cabal run 'n-queens-problem' 3
Problem has no solutions
```

### Запустить Unit тесты
```
$ cabal run 'unit-tests'
Cases: 24  Tried: 24  Errors: 0  Failures: 0
```