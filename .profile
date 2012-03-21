if [ $HOSTNAME == "ajfrank" ]; then
    xrandr --output DVI-I-2 --rotate left --pos 1921x0
    xrandr --output DVI-I-1 --pos 0x177
    feh --bg-scale ~/wallpapers/camel_1920x1200.jpg
elif [ $HOSTNAME == "lappy" ]; then
    xcalib ~/.icc/lappy.icc
    feh --bg-scale ~/pics/wallpapers/cloudy_day_1920x1200.jpg
fi

urxvtd -q -o -f
xset r rate 250 30 &
