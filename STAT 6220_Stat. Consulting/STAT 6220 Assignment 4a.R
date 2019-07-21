jpeg(filename="Test_Bar_Lab.jpeg")
plot(bar_lab)
dev.off()

postscript(file="Test_PS.ps")
plot(bar_lab)
dev.off()

png(file="Test_PNG.png")
plot(bar_lab)
dev.off()

getwd()
pdf("Test_PDF.pdf")
plot(bar_lab, title = "Count of Complete vs Incomplete Lab Sets \n Among New Patients, By Year")
dev.off()

bar_lab <- ggplot(labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#ccb85f","#000000"))

bar_lab <- ggplot(labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "stack") + 
  ylab("Count") + 
  scale_fill_manual(values = c("#ccb85f","#000000"))

bar_lab <- ggplot(labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "stack") + ylab("Count") + 
  ggtitle("Count of Complete vs Incomplete Lab Sets \n Among New Patients, By Year") + 
  theme(plot.title = element_text(family = "Arial", size = 14, hjust = 0.5), text = element_text(size = 12, family = "Arial")) +
  scale_fill_manual(values = c("#ccb85f","#000000"))

bar_lab <- ggplot(labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "stack") + ylab("Count") + 
  ggtitle("Count of Complete vs Incomplete Lab Sets \n Among New Patients, By Year") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))
