# Users service

Simple AUTH API

## Endpoints

`POST /users`

Valid payload: 

``` 
{
  "email": "valid@email.com",
  "password": "validpassword1"
} 
```

Returns `201` if user is valid

Returns `400` if user is in the wrong format

Returns `422` if validation does not pass


## To run

From the `server` directory: 

`cabal install`

`cabal run`

## To run the tests

From the `server` directory: 

`cabal sandbox init`

`cabal install --enable-tests`

`cabal test`


