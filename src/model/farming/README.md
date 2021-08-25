# Crop Planting under Uncertainty

Consider a European farmer who specializes in raising wheat, corn, and sugar beets on his 500 acres of land. During the winter, he wants to decide how much land todevote to each crop. (We refer to the farmer as “he” for convenience and not to imply anything about the gender of European farmers.)

The farmer knows that at least 200 tons (T) of wheat and 240 T of corn are needed for cattle feed. These amounts can be raised on the farm or bought from a wholesaler. Any production in excess of the feeding requirement would be sold. Over the last decade, mean selling prices have been $170 and $150 per ton of wheat and corn, respectively. The purchase prices are 40% more than this due to the wholesaler’s margin and transportation costs.

Another profitable crop is sugar beet, which he expects to sell at $36/T; however, the European Commission imposes a quota on sugar beet production. Any amount in excess of the quota can be sold only at $10/T. The farmer’s quota for next year is 6000 T. Based on past experience, the farmer knows that the mean yield on his land isroughly 2.5 T, 3 T, and 20 T per acre for wheat, corn, and sugar beets, respectively.

After thinking about a simple solution to his concrete problem, the farmer becomes worried. He has indeed experienced quite different yields for the same crop over different years mainly because of changing weather conditions. Most crops need rain during the few weeks after seeding or planting, then sunshine is welcome for the rest of the growing period. Sunshine should, however, not turn into drought, which causes severe yield reductions. Dry weather is again beneficial during harvest. From all these factors, yields varying 20 to 25% above or below the mean yield are not unusual.

The farmer decides to write a GAMS program to implement a stochastic recourse model with yield as the random variable. Instead of hard coding all his data into GAMS (he has more crop options than just wheat, corn and sugar beet) he decides to build a generic model and a GAMS MIRO application and opens a little side business to sell this as a service to his fellow farmers.
