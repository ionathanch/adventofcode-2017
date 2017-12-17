p = 0;
arr = [0];
for (n = 1; n <= 2017; n++) {
    p = (p + 312 % n + 1) % n;
    arr.splice(p, 0, n);
}
console.log(arr[arr.indexOf(2017) + 1]);

p = 0;
oneth = 1;
for (n = 1; n <= 50000000; n++) {
    p = (p + 312 % n + 1) % n;
    if (p == 0) oneth = n;
}
console.log(oneth);