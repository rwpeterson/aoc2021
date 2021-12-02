{
    if ($1 > prev && NR > 1) {
        deeper++;
    }
    prev = $1;

    window[NR] += $1;
    window[NR - 1] += $1;
    window[NR - 2] += $1;
}
END {
    print "Number of measurements larger than previous:", deeper;

    for (i = 2; i <= NR - 2; i++) {
        if (window[i] > window[i - 1]) {
            deeper2++;
        }
    }
    print "Number of 3-measurement windows larger than previous:", deeper2;
}
