#include <iostream>
#include "chaine.h"
#include "sigle.h"

void tests()
{
    try
    {
        std::cout << "Partie 1" << std::endl;

        std::cout << "Test 1" << std::endl;

        std::cout << "Constructeur par defaut de Chaine" << std::endl;
        Chaine chaine1;
        std::cout << "Valeur de la chaine1" << std::endl;
        chaine1.print();
        std::cout << "Constructeur de copie de Chaine" << std::endl;
        Chaine chaine2(chaine1);
        std::cout << "Valeur de la chaine2 doit etre egale a la chaine1" << std::endl;
        chaine2.print();

        std::cout << "Test 2" << std::endl;

        std::cout << "Modification de la premiere lettre de la chaine1 par un Q" << std::endl;
        chaine1[0] = 'Q';
        chaine1.print();
        std::cout << "Operateur d'affectation de Chaine" << std::endl;
        chaine1 = chaine2;
        std::cout << "Valeur de la chaine1 doit etre egale a la chaine2" << std::endl;
        chaine1.print();
        std::cout << "Concatenation de chaine1 et chaine2 dans chaine4" << std::endl;
        Chaine chaine4 = chaine1 + chaine2;
        chaine4.print();

        std::cout << "Test 3" << std::endl;

        std::cout << "Chaine4 en majuscules" << std::endl;
        convertMaj(chaine4).print();
        std::cout << "Chaine4 en minuscules" << std::endl;
        convertMin(chaine4).print();
        std::cout << "Test anagramme entre chaine1 et chaine1" << std::endl;
        std::cout << estAnagramme(chaine1, chaine1) << std::endl;
        std::cout << "Test anagramme entre chaine1 et chaine2" << std::endl;
        std::cout << estAnagramme(chaine1, chaine2) << std::endl;
        std::cout << "Test anagramme entre chaine3 et chaine2" << std::endl;
        std::cout << estAnagramme(chaine4, chaine2) << std::endl;
        std::cout << "Test palindrome de chaine1" << std::endl;
        std::cout << estPalindrome(chaine1) << std::endl;

        std::cout << "Partie 2" << std::endl;

        std::cout << "Test 1" << std::endl;

        std::cout << "Constructeur par defaut de Sigle" << std::endl;
        Sigle sigle1;
        std::cout << "Valeur de sigle1" << std::endl;
        sigle1.print();
        std::cout << "Constructeur par copie de Sigle" << std::endl;
        Sigle sigle2(sigle1);
        std::cout << "Valeur de sigle2 doit etre egale a sigle1" << std::endl;
        sigle2.print();

        std::cout << "Test 2" << std::endl;

        std::cout << "Operateur d'affectation de Sigle" << std::endl;
        sigle1 = sigle2;
        std::cout << "Valeur de sigle1 doit etre egale a sigle2" << std::endl;
        sigle1.print();
        std::cout << "Concatenation de sigle1 et sigle2 dans sigle4" << std::endl;
        Sigle sigle4 = sigle1 + sigle2;
        sigle4.print();
        std::cout << "Affichage de la chaine de la lettre 2 de sigle 4" << std::endl;
        std::cout << sigle4[1] << std::endl;
        std::cout << "Modification de la chaine de la lettre 1 de sigle 4 par la chaine1" << std::endl;
        sigle4(0, chaine1);
        sigle4.print();
    }
    catch(std::exception& exception)
    {
        std::cerr << exception.what() << std::endl;
    }
}

int main(int argc, char *argv[])
{
    tests();

    return 0;
}
