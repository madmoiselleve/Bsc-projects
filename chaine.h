#ifndef CHAINE_H
#define CHAINE_H

#include <iostream>
#include <exception>

class Chaine
{
public:
    Chaine();
    Chaine(const Chaine& other);
    virtual ~Chaine();

    Chaine& operator=(const Chaine& other);
    char& operator[](int i);
    friend Chaine operator+(const Chaine& first, const Chaine& second);
    friend Chaine convertMaj(const Chaine& other);
    friend Chaine convertMin(const Chaine& other);

    friend bool estAnagramme(const Chaine& first, const Chaine& second);
    friend bool estPalindrome(const Chaine& other);

    void print();

protected:
    unsigned int taille;
    char* chaine;
};

Chaine operator+(const Chaine& first, const Chaine& second);

Chaine convertMaj(const Chaine& other);
Chaine convertMin(const Chaine& other);

bool estAnagramme(const Chaine& first, const Chaine& second);
bool estPalindrome(const Chaine& other);

#endif // CHAINE_H
