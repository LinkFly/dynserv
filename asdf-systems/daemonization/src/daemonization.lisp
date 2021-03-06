(defpackage :daemonization 
  (:use :cl :daemon-utils-port)
  (:export 
   ))

(in-package :daemonization)

#|
1. Получение аргументов.
2. Установка глобальных переменных.
-. Действия необходимые для отсоединения от терминала
     - определение константы sb-unix:tiocnotty
- Действия необходимые чтобы открыть зарезервированный порт (< 1024)
     - определение константы +PR_SET_KEEPCAPS+
3. Получение конфигурационных параметров.
4. Проверка на корректность комманды.
5. Обработка комманд.
    - zap - удалить pidfile и выйти.
    - stop - остановить демон и выйти
       Остановить демон:
       - прочитать pid 
       - послать пользовательский сигнал
       - в цикле посылать сигнал 0, как только появляется отсутствие ошибки - выйти из цикла
       - удалить файл pidfile.
    - restart - остановить демон, и далее стартовать демон.
       Стартовать демон:
       - определить сл.системные ф-ии: grantpt unlockpt ptsname initgroups, prctl, cap_from_text cap_set_proc cap_free
       - установить обработчик(один и тот же) на сигналы: sigusr1, sigchld
       - установить флаг сохранения возможностей процесса, после смены идентификаторов
       - сменить пользователя
       - установить возможность CAP_NET_BIND_SERVICE=ep
       - демонизация
          - fork
             - fork
             - сравнение с нулём, 
                 - если нуль то это потомок, перепрыгиваем и идём дальше
                 - если нет, то
                      - создаём цикл, и не выходим пока статус = nil
                      - как только *status* не равен nil
                            - если равен sigusr1, то выходим с успехом (0)
                            - если не равен sigusr1 то выходим с ошибкой (1)
        Далее, для потомка:
          - получаем ppid
          - меняем *debugger-hook* на глобальный, свой, обработчик ошибок
          - меняем обработчики (обратно) на :default
          - изменяем текущую директорию
          - выставляем маску для создаваемых файлов
          - отсоединяемся от терминала
          - переключаемся к псевдотерминалу
          - создание нового сенса (setsid)

Далее, пользовательские процедуры.
 - настройка ASDF
 - настройка SWANK
 - загрузка restas
 - настройка restas
 - загрузка дополнительных ASDF-систем
 - запуск сайтов

          - опять поменять обработчик сигнала sigusr1
             - реакция на остановку демона - логгирование и обработка возможной ошибки.
             - 
          - записать pid в *pidfile*
          - послать родителю сигнал sigusr1
          - установить *debugger-hook к nil
          - логирование о успешном запуске


Что делает no-daemon?
1. Получает аргументы
2. Загружает конфиг
3. Проверяет комманду
4. Устанавливает флаг сохранения возможностей
5. Меняет пользователя
6. Устанавливает CAP_NET_BIND_SERVICE=ep
7. Изменяет текущую директорию
8. Устанавливает маску для создаваемых файлов
9. asdf,swank,restas,sites


Что связано с fork?
1. Установка обработчика signal-handler сигналов sigusr1 и sigchld (для родителя)
2. Fork и ожидание изменение статуса потомком, затем выход (для родителя)
Далее для потомка:
3. Снятие обработчиков сигналов sigusr1, sigchld (установка :default)
- Установка нового обработчика sigusr1 для остановки демона в дальнейшем
4. Сигнал sigusr1 родителя об успешном старте.
|#

