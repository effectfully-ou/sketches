# validation-leak

`./leak.sh` produces

```
500000500000
      72,666,928 bytes allocated in the heap
     125,238,824 bytes copied during GC
      36,110,016 bytes maximum residency (7 sample(s))
       6,854,976 bytes maximum slop
              87 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       132 colls,     0 par    0.040s   0.048s     0.0004s    0.0009s
  Gen  1         7 colls,     0 par    0.044s   0.055s     0.0078s    0.0275s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.016s  (  0.017s elapsed)
  GC      time    0.084s  (  0.103s elapsed)
  EXIT    time    0.000s  (  0.004s elapsed)
  Total   time    0.124s  (  0.124s elapsed)

  %GC     time      67.7%  (83.1% elapsed)

  Alloc rate    4,541,683,000 bytes per MUT second

  Productivity  32.3% of total user, 16.7% of total elapsed
```

`./fine.sh` produces

```
500000500000
      80,053,448 bytes allocated in the heap
         270,200 bytes copied during GC
          44,384 bytes maximum residency (2 sample(s))
          53,352 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       152 colls,     0 par    0.000s   0.000s     0.0000s    0.0002s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.008s  (  0.009s elapsed)
  GC      time    0.000s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.032s  (  0.010s elapsed)

  %GC     time       0.0%  (6.0% elapsed)

  Alloc rate    10,006,681,000 bytes per MUT second

  Productivity 100.0% of total user, 92.4% of total elapsed
```
