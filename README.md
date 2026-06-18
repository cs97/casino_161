# easy start
```
cd build
```
```
./casino_server_linux_x86_64
```

http://127.0.0.1:3000/






# casino_161
build -> release
```
cd build
```

```
elm make main.elm --output=elm.js
```


# backend api

```
curl -X POST http://127.0.0.1:3000/score/spieler1  -H "Content-Type: application/json" -d '{"score": 13}'
```
```
curl http://127.0.0.1:3000/score/spieler1
```


# build backen 
```
cd backend
```
```
cargo build --release
```
```
cd ..
```
```
./backend/target/build/casino_backend
```
