read -p "Press any key to start the Hello World demo!"
echo "Compiling Shapeshifter..."
cd ..
make -s

echo "Done!"

# Hello World
read -p "Let's see some Hello World action. Please any key to continue."
./shapeshifter.native < hello_world_demo/hello_world.shift > demo.ll
lli demo.ll


# MAYBE ANOTHER DEMO HERE? 

rm demo.ll

# echo "One more thing..."
# ./shapeshifter.native < hello_world_demo/jacob.shift > jacob.ll
# lli jacob.ll

# rm jacob.ll

echo -e "\nThanks for watching!"
make clean -s 

