Erlang Appserver
================

version 0.1.1

Прототип многопользовательского сервера общего назначения.

Установка на чистую систему (в качестве примера Ubuntu Server).
 
Установка Erlang
```
sudo apt-get install Erlang
```

Установка Git
```
sudo apt-get install Git
```

Установка Rebar
```
git clone git://github.com/basho/rebar.git && cd rebar && ./bootstrap
sudo ln -s `pwd`/rebar /usr/bin/rebar
```
Установка MongoDB
```
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/10gen.list
sudo apt-get update
sudo apt-get install mongodb-10gen
```

Установка и запуск Erlang Appserver
```
git clone git://github.com/Jugastrov/Erlang-Appserver.git && cd Erlang-Appserver
make all
```
Сервер доступен на localhost:8008


