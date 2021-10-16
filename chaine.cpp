#include "chaine.h"

Chaine::Chaine() : taille(0), chaine(nullptr)
{
    std::cout << "Entrer la taille de la chaine: ";
    std::cin >> taille;

    if(taille <= 0)
        throw std::invalid_argument("Taille non strictement positive");

    chaine = new char[taille];

    std::cout << "Entrer chaque lettre de la chaine: ";
    for(unsigned int i = 0; i < taille; ++i)
        std::cin >> chaine[i];
}

Chaine::Chaine(const Chaine& other) : taille(other.taille), chaine(nullptr)
{
    chaine = new char[taille];
    for(unsigned int i = 0; i < taille; ++i)
        chaine[i] = other.chaine[i];
}

Chaine::~Chaine()
{
    if(chaine != nullptr)
        delete[] chaine;
}

Chaine& Chaine::operator=(const Chaine& other)
{
    if(this != &other)
    {
        if(chaine != nullptr)
            delete[] chaine;

        taille = other.taille;
        chaine = new char[taille];
        for(unsigned int i = 0; i < taille; ++i)
            chaine[i] = other.chaine[i];
    }

    return *this;
}

char& Chaine::operator[](int i)
{
    if(i >= taille)
        throw std::invalid_argument("Index invalide");

    return chaine[i];
}

void Chaine::print()
{
    for(unsigned int i = 0; i < taille; ++i)
    {
        std::cout << chaine[i];
    }
    std::cout << std::endl;
}

Chaine operator+(const Chaine& first, const Chaine& second)
{
    Chaine result(first);
    delete[] result.chaine;

    result.chaine = new char[first.taille + second.taille];

    for(unsigned int i = 0; i < first.taille; ++i)
        result.chaine[i] = first.chaine[i];
    for(unsigned int i = 0; i < second.taille; ++i)
        result.chaine[first.taille+i] = second.chaine[i];

    result.taille += second.taille;

    return result;
}

Chaine convertMaj(const Chaine& other)
{
    Chaine result = other;
    for(unsigned int i = 0; i < result.taille; ++i)
    {
        if(result[i] >= 'a' && result[i] <= 'z')
        {
            result[i] = result[i] - 'a' + 'A';
        }
    }
    return result;
}

Chaine convertMin(const Chaine& other)
{
    Chaine result = other;
    for(unsigned int i = 0; i < result.taille; ++i)
    {
        if(result[i] >= 'A' && result[i] <= 'Z')
        {
            result[i] = result[i] - 'A' + 'a';
        }
    }
    return result;
}

bool estAnagramme(const Chaine& first, const Chaine& second)
{
    if(first.taille != second.taille)
        return false;

    unsigned int comparaisons[26];
    for(unsigned int i = 0; i < 26; ++i)
        comparaisons[i] = 0;

    for(unsigned int i = 0; i < first.taille; ++i)
    {
        if(first.chaine[i] >= 'a' && first.chaine[i] <= 'z')
            comparaisons[first.chaine[i]-'a'] += 1;
        else if(first.chaine[i] >= 'A' && first.chaine[i] <= 'Z')
            comparaisons[first.chaine[i]-'A'] += 1;

        if(second.chaine[i] >= 'a' && second.chaine[i] <= 'z')
            comparaisons[second.chaine[i]-'a'] -= 1;
        else if(second.chaine[i] >= 'A' && second.chaine[i] <= 'Z')
            comparaisons[second.chaine[i]-'A'] -= 1;
    }

    for(unsigned int i = 0; i < 26; ++i)
    {
        if(comparaisons[i] != 0)
            return false;
    }

    return true;
}

bool estPalindrome(const Chaine& other)
{
    for(unsigned int i = 0; i < other.taille/2; ++i)
    {
        if(other.chaine[i] != other.chaine[other.taille-i])
            return false;
    }

    return true;
}
