#include "sigle.h"

Sigle::Sigle() : Chaine()
{
    std::cout << "Entrer les " << taille << " chaine(s) du sigle:" << std::endl;

    for(unsigned int i = 0; i < taille; ++i)
    {
        if(chaine[i] >= 'a' && chaine[i] <= 'z')
            chaine[i] = chaine[i] - 'a' + 'A';

        sigle.push_back(convertMin(Chaine()));

        if(sigle[i][0] >= 'a' && sigle[i][0] <= 'z')
                sigle[i][0] = sigle[i][0] - 'a' + 'A';

        if(chaine[i] != sigle[i][0])
            throw std::invalid_argument("La chaine ne demarre pas par le meme caractere que le sigle");
    }
}

Sigle::Sigle(const Sigle& other) : Chaine(other), sigle(other.sigle)
{

}

Sigle::~Sigle()
{

}

Sigle& Sigle::operator=(const Sigle& other)
{
    if(this != &other)
    {
        if(chaine != nullptr)
            delete[] chaine;

        taille = other.taille;
        chaine = new char[taille];
        for(unsigned int i = 0; i < taille; ++i)
            chaine[i] = other.chaine[i];
        sigle = other.sigle;
    }

    return *this;
}

Sigle operator+(const Sigle& first, const Sigle& second)
{
    Sigle result(first);

    delete[] result.chaine;
    result.chaine = new char[first.taille + second.taille];

    for(unsigned int i = 0; i < first.taille; ++i)
        result.chaine[i] = first.chaine[i];

    for(unsigned int i = 0; i < second.taille; ++i)
    {
        result.chaine[first.taille+i] = second.chaine[i];
        result.sigle.push_back(second.sigle[i]);
    }

    result.taille += second.taille;

    return result;
}

char const& Sigle::operator[](int i) const
{
    if(i >= taille)
        throw std::invalid_argument("Index invalide");

    return chaine[i];
}

Chaine& Sigle::operator()(int i, Chaine& other)
{
    if(i >= taille)
        throw std::invalid_argument("Index invalide");

    sigle[i] = convertMin(other);
    chaine[i] = other[0];

    if(chaine[i] >= 'a' && chaine[i] <= 'z')
        chaine[i] = chaine[i] - 'a' + 'A';

    if(sigle[i][0] >= 'a' && sigle[i][0] <= 'z')
        sigle[i][0] = sigle[i][0] - 'a' + 'A';

    return *this;
}

void Sigle::print()
{
    Chaine::print();
    for(unsigned int i = 0; i < taille; ++i)
    {
        std::cout << " - ";
        sigle[i].print();
    }
}
