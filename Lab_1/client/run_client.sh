stack build
for i in {1..1000}
  do
   # curl http://localhost:8000/echo.php?message=hi
   stack exec client-exe 
   sleep 1
  done
