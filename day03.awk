function bin2dec(b) {
    n = length(b)
    d = 0;
    for (i = 1; i <= n; i++) {
        if (substr(b, i, 1) == "1") {
            d += 2 ** (n - i);
        }
    }
    return d
}
BEGIN {
    FS = "";
}
{
    for (i = 1; i <= NF; i++) {
        if ($i == "1") {
            ones[i]++;
        }
        data[NR][i] = $i;
        data2[NR][i] = $i;
    }
    if (NF > max) {
        max = NF;
    }
}
END {
    for (one in ones) {
        if (ones[one] > NR / 2) {
            gamma = gamma "1";
            epsilon = epsilon "0";
        } else {
            gamma = gamma "0";
            epsilon = epsilon "1";
        }
    }
    g = bin2dec(gamma);
    e = bin2dec(epsilon);
    print "gamma", g;
    print "epsilon", e;
    print "gamma * epsilon =", g * e;

    for (bit = 1; bit <= max; bit++) {
        ob = 0;
        ob2 = 0;
        for (line in data) {
            if (data[line][bit] == "1") {
                ob++;
            }
        }
        for (line in data2) {
            if (data2[line][bit] == "1") {
                ob2++;
            }
        }
        if (ob >= NR / 2) {
            oxygen = oxygen "1";
            for (line in data) {
                if (data[line][bit] == "0") {
                    delete data[line];
                }
            }
        } else {
            oxygen = oxygen "0";
            for (line in data) {
                if (data[line][bit] == "1") {
                    delete data[line];
                }
            }
        }
        if (ob2 >= NR / 2) {
            co2 = co2 "0";
            for (line in data2) {
                if (data2[line][bit] == "1") {
                    delete data2[line];
                }
            }
        } else {
            co2 = co2 "1";
            for (line in data2) {
                if (data2[line][bit] == "0") {
                    delete data2[line];
                }
            }
        }
    }
    o = bin2dec(oxygen);
    c = bin2dec(co2);
    print "oxygen", oxygen, o;
    print "co2", co2, c;
    print "oxygen * co2 =", o * c;
}
