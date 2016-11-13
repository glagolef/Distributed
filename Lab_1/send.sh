for i in {1..1000}
  do
   # curl http://localhost:8000/echo.php?message=hi
   ./client 
   sleep 1
  done
