pdf(paste0(my_path_root,"/data/output/Hulth2003/curves_comparison_Hulth_1512.pdf"),width=8,height=8,paper="a4r")

par(oma=c(0,0,2,0), mfrow=c(2,2), mar=c(6,4,6,4))

plot(1:length(diff(my_levels)), as.numeric(diff(my_levels)), xlab = "shells", ylab="difference in shell size", type="b", xaxt="n", main="inflexion")
axis(1, at=1:length(diff(my_levels)), labels=paste0("(",names(diff(my_levels))," - ", names(my_levels)[1:(length(my_levels)-1)],")"))
grid(lwd=1.5)
abline(h=0)
points(2, 3, col="red", pch=15)

plot(sorted_density_per_level,  xaxt = "n", ylab="density", type="b", main="density", xlab="k-cores")
axis(side=1,at=1:length(sorted_density_per_level),labels=paste0(names(sorted_density_per_level),"-core"))
grid(lwd=1.5)
points(2, sorted_density_per_level[2], col="red", pch=15)

plot(1:length(all_nodes_tr),all_nodes_tr, xlab="nodes", ylab="PR scores", type="b", main="PageRank")
grid(lwd=1.5)
points(2,all_nodes_tr[2], pch=15, col="red")
points(5,all_nodes_tr[5], pch=17, col="blue")
legend("topright", legend=c("elbow","top 33%"), pch=c(15,17), col=c("red","blue"))

plot(1:length(sorted_sum_core_no),sorted_sum_core_no, xlab="nodes", ylab="CR scores", type="b", main="CoreRank")
grid(lwd=1.5)
points(10,sorted_sum_core_no[10], pch=15, col="red")
points(5,sorted_sum_core_no[5], pch=17, col="blue")
legend("topright", legend=c("elbow","top 33%"), pch=c(15,17), col=c("red","blue"))

mtext("Hulth 1512", outer=TRUE)

dev.off()




pdf(paste0(my_path_root,"/data/output/Hulth2003/curves_comparison_Hulth_1497.pdf"),width=8,height=8,paper="a4r")

par(oma=c(0,0,2,0), mfrow=c(2,2), mar=c(6,4,6,4))

plot(1:length(diff(my_levels)), as.numeric(diff(my_levels)), xlab = "shells", ylab="difference in shell size", type="b", xaxt="n", main="inflexion")
axis(1, at=1:length(diff(my_levels)), labels=paste0("(",names(diff(my_levels))," - ", names(my_levels)[1:(length(my_levels)-1)],")"))
grid(lwd=1.5)
abline(h=0)
points(1, 5, col="red", pch=15)

plot(sorted_density_per_level,  xaxt = "n", ylab="density", type="b", main="density", xlab="k-cores")
axis(side=1,at=1:length(sorted_density_per_level),labels=paste0(names(sorted_density_per_level),"-core"))
grid(lwd=1.5)
points(2, sorted_density_per_level[2], col="red", pch=15)

plot(1:length(all_nodes_tr),all_nodes_tr, xlab="nodes", ylab="PR scores", type="b", main="PageRank")
grid(lwd=1.5)
points(4,all_nodes_tr[4], pch=15, col="red")
points(8,all_nodes_tr[8], pch=17, col="blue")
legend("topright", legend=c("elbow","top 33%"), pch=c(15,17), col=c("red","blue"))

plot(1:length(sorted_sum_core_no),sorted_sum_core_no, xlab="nodes", ylab="CR scores", type="b", main="CoreRank")
grid(lwd=1.5)
points(5,sorted_sum_core_no[5], pch=15, col="red")
points(8,sorted_sum_core_no[8], pch=17, col="blue")
legend("topright", legend=c("elbow","top 33%"), pch=c(15,17), col=c("red","blue"))

mtext("Hulth 1497", outer=TRUE)

dev.off()



