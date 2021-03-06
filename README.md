![kurt logo][logo]


Kurt is a [Computer Go][computer_go] program written in Haskell.

It is named after the logician [Kurt Gödel][kurt_goedel] and written by
[Fabian Linzberger][lefant_net].


Source code is [available online on github][github_kurt]. 

[![Build Status](https://travis-ci.org/lefant/kurt.png)](https://travis-ci.org/lefant/kurt)

Comments and contributions always welcome!


It has also played online on the Kiseido go server ([KGS][kgs]) with
the nick "kurtBot". It is only playing on 9x9 and its strength there
is about 25 kyu.


KGS also provides an archive of [all of kurtBots games][kgs_kurt].



## running stdin/stdout gtp through docker (for example to attach gogui)

```
docker run -i lefant/kurt
```


## running through docker-compose on kgs

(adjust env variables!)
```
NAME=myBot PASSWORD=secret KGSGTP_ARGS='opponent=lefant' docker-compose up
```


[computer_go]: http://en.wikipedia.org/wiki/Computer_Go
[kurt_goedel]: http://en.wikipedia.org/wiki/Kurt_G%C3%B6del
[lefant_net]: http://lefant.net/
[github_kurt]: http://github.com/lefant/kurt
[kgs]: http://www.gokgs.com/
[kgs_kurt]: http://www.gokgs.com/gameArchives.jsp?user=kurtBot&oldAccounts=y
[logo]: https://raw.githubusercontent.com/lefant/kurt/master/kurt-logo.jpg
