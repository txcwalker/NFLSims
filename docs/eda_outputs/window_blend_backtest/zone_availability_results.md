# Zone-share availability-gate backtest (2024 + 2025)

Red-zone / goal-line target_share + carry_share. Pooled over the last 8 TEAM games. `n_active` = games of those 8 he appeared in. Weighted by team zone plays in the target week. Preseason = his prior-season zone share.


## ACTIVE in the target week (primary) - rows=9576

| kind | zone | n | preseason | pooled8 | play_gate5 | avail_4 | avail_5 | avail_6 | avail_7 | availpool_4 | availpool_5 | availpool_6 | availpool_7 | avail6_a0.15 | avail6_a0.3 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| carry_share | goalline | 1240 | 0.2639 | 0.2494 | 0.2524 | 0.2501 | 0.2503 | 0.2515 | 0.2567 | 0.2559 | 0.2546 | 0.2538 | 0.2583 | 0.2500 | 0.2497 |
| carry_share | redzone | 2271 | 0.1887 | 0.1758 | 0.1820 | 0.1774 | 0.1783 | 0.1786 | 0.1817 | 0.1782 | 0.1785 | 0.1793 | 0.1819 | 0.1771 | 0.1769 |
| target_share | goalline | 1711 | 0.2060 | 0.2072 | 0.2081 | 0.2071 | 0.2079 | 0.2090 | 0.2089 | 0.2122 | 0.2110 | 0.2111 | 0.2097 | 0.2073 | 0.2058 |
| target_share | redzone | 4354 | 0.1233 | 0.1214 | 0.1264 | 0.1222 | 0.1218 | 0.1219 | 0.1232 | 0.1243 | 0.1235 | 0.1232 | 0.1237 | 0.1213 | 0.1209 |

### aggregate vs pooled8_ungated (mean of 4 cells; negative = better)

| method | rel_vs_ref | lo | hi |
|---|---|---|---|
| avail6_a0.3 | -0.0009 | -0.0164 | 0.0156 |
| pooled8 | 0.0000 | 0.0000 | 0.0000 |
| avail6_a0.15 | 0.0023 | -0.0125 | 0.0190 |
| avail_4 | 0.0044 | -0.0043 | 0.0133 |
| avail_5 | 0.0061 | -0.0057 | 0.0176 |
| avail_6 | 0.0094 | -0.0054 | 0.0267 |
| availpool_6 | 0.0178 | 0.0016 | 0.0346 |
| availpool_5 | 0.0179 | 0.0053 | 0.0310 |
| avail_7 | 0.0214 | 0.0054 | 0.0387 |
| availpool_4 | 0.0219 | 0.0107 | 0.0327 |
| play_gate5 | 0.0232 | 0.0031 | 0.0442 |
| availpool_7 | 0.0255 | 0.0089 | 0.0423 |
| preseason | 0.0354 | 0.0100 | 0.0678 |

## ALL rows (as in the first zone run) - rows=12780

| kind | zone | n | preseason | pooled8 | play_gate5 | avail_4 | avail_5 | avail_6 | avail_7 | availpool_4 | availpool_5 | availpool_6 | availpool_7 | avail6_a0.15 | avail6_a0.3 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| carry_share | goalline | 1594 | 0.2548 | 0.2295 | 0.2472 | 0.2422 | 0.2433 | 0.2443 | 0.2480 | 0.2523 | 0.2491 | 0.2472 | 0.2497 | 0.2432 | 0.2431 |
| carry_share | redzone | 3094 | 0.1838 | 0.1587 | 0.1806 | 0.1750 | 0.1762 | 0.1771 | 0.1796 | 0.1813 | 0.1792 | 0.1793 | 0.1803 | 0.1759 | 0.1756 |
| target_share | goalline | 2149 | 0.1918 | 0.1869 | 0.1944 | 0.1923 | 0.1932 | 0.1938 | 0.1945 | 0.2010 | 0.1985 | 0.1965 | 0.1954 | 0.1924 | 0.1913 |
| target_share | redzone | 5943 | 0.1129 | 0.1033 | 0.1162 | 0.1102 | 0.1110 | 0.1117 | 0.1129 | 0.1146 | 0.1138 | 0.1135 | 0.1136 | 0.1113 | 0.1110 |

### aggregate vs pooled8_ungated (mean of 4 cells; negative = better)

| method | rel_vs_ref | lo | hi |
|---|---|---|---|
| pooled8 | 0.0000 | 0.0000 | 0.0000 |
| avail_4 | 0.0634 | 0.0433 | 0.0846 |
| avail6_a0.3 | 0.0659 | 0.0415 | 0.0957 |
| avail6_a0.15 | 0.0686 | 0.0431 | 0.0983 |
| avail_5 | 0.0692 | 0.0467 | 0.0930 |
| avail_6 | 0.0745 | 0.0489 | 0.1042 |
| avail_7 | 0.0863 | 0.0598 | 0.1153 |
| availpool_6 | 0.0890 | 0.0622 | 0.1193 |
| availpool_7 | 0.0922 | 0.0658 | 0.1215 |
| availpool_5 | 0.0943 | 0.0692 | 0.1199 |
| play_gate5 | 0.0949 | 0.0673 | 0.1270 |
| preseason | 0.0966 | 0.0645 | 0.1376 |
| availpool_4 | 0.1064 | 0.0839 | 0.1293 |

## Active in week t, full attendance (active 8 of 8) - rows=3424

| kind | zone | n | preseason | pooled8 | play_gate5 | avail_6 | availpool_6 | availpool_4 |
|---|---|---|---|---|---|---|---|---|
| carry_share | goalline | 518 | 0.2820 | 0.2702 | 0.2676 | 0.2702 | 0.2702 | 0.2702 |
| carry_share | redzone | 890 | 0.1900 | 0.1768 | 0.1777 | 0.1768 | 0.1768 | 0.1768 |
| target_share | goalline | 606 | 0.2114 | 0.2205 | 0.2145 | 0.2205 | 0.2205 | 0.2205 |
| target_share | redzone | 1410 | 0.1362 | 0.1370 | 0.1419 | 0.1370 | 0.1370 | 0.1370 |

## Active in week t, missed >=1 of the last 8 - rows=6152

| kind | zone | n | preseason | pooled8 | play_gate5 | avail_6 | availpool_6 | availpool_4 |
|---|---|---|---|---|---|---|---|---|
| carry_share | goalline | 722 | 0.2507 | 0.2342 | 0.2414 | 0.2379 | 0.2418 | 0.2455 |
| carry_share | redzone | 1381 | 0.1878 | 0.1751 | 0.1848 | 0.1799 | 0.1810 | 0.1791 |
| target_share | goalline | 1105 | 0.2031 | 0.2000 | 0.2046 | 0.2027 | 0.2060 | 0.2076 |
| target_share | redzone | 2944 | 0.1174 | 0.1143 | 0.1193 | 0.1150 | 0.1168 | 0.1185 |