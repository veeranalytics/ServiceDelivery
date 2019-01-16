# Service Delivery
## Call Centre Capacity Modeling using Erlang C Formula

The Erlang C formula is used to calculate the chance of queued calls (waiting to be answered). This can be used to 
determine how many # operators/agents are needed to answer all arriving calls.

The Erlang C formula does not consider call abandonment: it assumes that all callers will remain in the queue until they 
are being served. 

In reality, some callers will abandon when the waiting time exceeds their patience. This has the effect of improving the 
waiting time for subsequent callers in the queue. Abandonment will therefore improve the service level for remaining 
callers. The net effect of this is, that the Erlang C formula will essentially overestimate the numbers of operators/agents 
that are needed.

#### The following terms are used in the R Code associated with Erlang C formula:
* rate-- Number of arrivals per interval
* duration-- Average handling time in seconds
* interval-- Length of interval in minutes (default = 60)
* agents-- Number of available agents
* target -- Acceptable waiting time
* gos_target-- Service level goal, the percentage of calls answered within the acceptable waiting time
* intensity– traffic intensity/workload
* erlang_c-- the chance of a queued call
* service_level-- the percentage of calls that are answered within the acceptable waiting time
* resources-- the number of needed agents for SL goal



