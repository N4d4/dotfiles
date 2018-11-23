#!/bin/bash

# EXTRACT THE WAL GENERATED COLOR
color_0=$(sed -n '1p' < ~/.cache/wal/colors)
color_1=$(sed -n '2p' < ~/.cache/wal/colors)
color_2=$(sed -n '3p' < ~/.cache/wal/colors)
color_3=$(sed -n '4p' < ~/.cache/wal/colors)
color_4=$(sed -n '5p' < ~/.cache/wal/colors)
color_5=$(sed -n '6p' < ~/.cache/wal/colors)
color_6=$(sed -n '7p' < ~/.cache/wal/colors)
color_7=$(sed -n '8p' < ~/.cache/wal/colors)
color_8=$(sed -n '9p' < ~/.cache/wal/colors)
color_9=$(sed -n '10p' < ~/.cache/wal/colors)
color_10=$(sed -n '11p' < ~/.cache/wal/colors)
color_11=$(sed -n '12p' < ~/.cache/wal/colors)
color_12=$(sed -n '13p' < ~/.cache/wal/colors)
color_13=$(sed -n '14p' < ~/.cache/wal/colors)
color_14=$(sed -n '15p' < ~/.cache/wal/colors)
color_15=$(sed -n '16p' < ~/.cache/wal/colors)
color_11_mod=$(sed -n '12p' < ~/.cache/wal/colors | tr -d "#")
color_9_mod=$(sed -n '10p' < ~/.cache/wal/colors | tr -d "#")
tmp_output="/tmp/polgen_tmp_output"
final_output="${HOME}/.cache/wal/generated_polybar"

# GENERATE THE NEW POLYBAR CONFIG
cp ~/.dotfiles/polybar/.config/polybar/config ${tmp_output}
sed -i "s_#04080B_${color_0}_g" ${tmp_output}
sed -i "s_#074C33_${color_1}_g" ${tmp_output}
sed -i "s_#0D612F_${color_2}_g" ${tmp_output}
sed -i "s_#076B4B_${color_3}_g" ${tmp_output}
sed -i "s_#16B752_${color_4}_g" ${tmp_output}
sed -i "s_#099B62_${color_5}_g" ${tmp_output}
sed -i "s_#A69167_${color_6}_g" ${tmp_output}
sed -i "s_#4DC48F_${color_7}_g" ${tmp_output}
sed -i "s_#666666_${color_8}_g" ${tmp_output}

sed -i "s_#4dC48F_${color_7}_g" ${tmp_output}
sed -i "s_#5A076B4B_#5A${color_11_mod}_g" ${tmp_output}
sed -i "s_#B3076B4B_#B3${color_11_mod}_g" ${tmp_output}
sed -i "s_#B3074C33_#B3${color_9_mod}_g" ${tmp_output}
sed -i "s_#5A074C33_#5A${color_9_mod}_g" ${tmp_output}

# CHECK POLYBAR PROCESS
case "$(pidof polybar | wc -w)" in
    0)  echo -e "[!] Running new instance of polybar"
    ;;
    *)  echo -e "[!] Restarting polybar"
        kill $(pidof polybar)
    ;;
esac

mv "${tmp_output}" "${final_output}"
polybar -r top -c ${final_output} & polybar -r bottom -c ${final_output} &

# test xmonad border
#cp ~/.xmonad/xmonad.hs /tmp/xmonad_test.hs
#echo -e "$(grep -n -m1 "myNormalBorderColor" < /tmp/xmonad_test.hs)" 
#num_line_to_change="$(cat /tmp/xmonad_test.hs |\
#                      grep -n -m1 "myNormalBorderColor  \=" |\
#                      grep -Eo '^[^:]+' \
#                     )"
#echo -e "numline = $num_line_to_change"
#sed -ie "${num_line_to_change}s_.*_myNormalBorderColor  = \"${color_0}\"_" /tmp/xmonad_test.hs
#echo -e "$(grep -n -m1 "myNormalBorderColor" < /tmp/xmonad_test.hs)" 
