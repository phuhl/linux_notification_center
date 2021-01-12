notify-send.py "With link support ❤️" "Best side: <a href=\"https://zombo.com\">zombo.com</a>" -a Firefox

notify-send.py "Now with markup" "The <b>longest</b> <u>yeah</u> <i>boiiiiiiiii!!</i>" -a notify-send

notify-send.py "We can do fancy progress" -a Volume --hint int:has-percentage:40

notify-send.py "And buttons" "Do you like buttons?" \
               --hint boolean:action-icons:false \
               --action Yes:Yes "Of course!":"Of course!" \
               -a Buttonsender &

