library(googlesheets4)

values <- read.csv('output/values_sheet.csv')
write_sheet(values, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'values')

agg <- read.csv('output/values_agg_sheet.csv')
write_sheet(agg, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'aggregates')

progress <- read.csv('output/progress_sheet.csv')
write_sheet(progress, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'progress')

meta <- read.csv('output/meta_sheet.csv')
write_sheet(meta, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'meta')

meta.extended <- read.csv('output/meta_extended_sheet.csv')
write_sheet(meta.extended, ss = '14kDeXh0H6vjzldndSBN4p67CCNANHdm7EmqnwojcKh0', sheet = 'meta_extended')
