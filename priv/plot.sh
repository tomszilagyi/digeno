# Create plots of convergence for all converg_<name>.log
# files where <name> is an identifier of the problem

mk_plt() {
    cat > $plt <<EOF
set terminal pngcairo size 750,450 enhanced font "Verdana,10"
set output "converg_$1.png"

set title "DiGenO Convergence ($1)"
set xlabel "Reductions"
set ylabel "Fitness"

set datafile separator ","
unset key
set grid
set logscale y
plot "converg_$1.log" using 1:2 with lines ls 1
EOF
}

plot() {
    plt=plot_$$.plt
    mk_plt $1
    gnuplot $plt
    rm $plt
}

for i in $(ls converg_*.log); do
    name=$(echo $i | sed 's/^converg_//' | sed 's/.log$//')
    plot $name;
done
