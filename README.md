# snakeoil

A service that does something

## Сборка

Для запуска процесса сборки достаточно выполнить просто:

    make

Чтобы запустить полученную сборку в режиме разработки и получить стандартный [Erlang shell][2], нужно всего лишь:

    make start

> _Хозяйке на заметку._ При этом используется стандартный Erlang релиз, собранный при помощи [relx][3] в режиме разработчика.

Рекомендуется вести разработку и сборку проекта в рамках локальной виртуальной среды, предоставляемой [wercker][1]. Настоятельно рекомендуется прогоны тестовых сценариев проводить только в этой среде.

    $ wercker dev

> _Хозяйке на заметку._ В зависимости от вашего окружения и операционной системы вам может понадобиться [Docker Machine][4].

## Документация

Дальнейшую документацию можно почерпнуть, пройдясь по ссылкам в [соответствующем документе](doc/index.md). 

[1]: http://devcenter.wercker.com/learn/basics/the-wercker-cli.html
[2]: http://erlang.org/doc/man/shell.html
[3]: https://github.com/erlware/relx
[4]: https://docs.docker.com/machine/install-machine/
