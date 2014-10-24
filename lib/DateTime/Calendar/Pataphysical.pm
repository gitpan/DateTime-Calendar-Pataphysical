package DateTime::Calendar::Pataphysical;

use strict;

use vars qw($VERSION);

$VERSION = '0.03';

#use DateTime 0.08;
use DateTime::Duration;
use DateTime::Locale;
use Params::Validate qw/validate SCALAR OBJECT/;

sub _floor {
    my $x  = shift;
    my $ix = int $x;
    if ($ix <= $x) {
        return $ix;
    } else {
        return $ix - 1;
    }
}

use overload ( 'fallback' => 1,
               '<=>' => '_compare_overload',
               'cmp' => '_compare_overload',
               '-' => '_subtract_overload',
               '+' => '_add_overload',
             );

{
    my $DefaultLocale;
    sub DefaultLocale {
        my $class = shift;

        if (@_) {
            my $lang = shift;

            DateTime::Locale->load($lang);

            $DefaultLocale = $lang;
        }

        return $DefaultLocale;
    }
}
__PACKAGE__->DefaultLocale('French');

sub new {
    my $class = shift;
    my %p = validate( @_,
                      { year  => {type => SCALAR},
                        month => {type => SCALAR, default => 1},
                        day   => {type => SCALAR, default => 1},
                        rd_secs   => { type => SCALAR, default => 0},
                        rd_nano   => { type => SCALAR, default => 0},
                        locale  => { type => SCALAR | OBJECT,
                                       default => $class->DefaultLocale },
                      } );

    my $self = bless \%p, $class;
    $self->{locale} = DateTime::Locale->load($p{locale})
        unless (ref $self->{locale});

    return $self;
}

sub clone {
    my $self = shift;

    return bless {%$self}, ref $self;
}

sub set
{
    my $self = shift;
    my %p = validate( @_,
                      { year     => { type => SCALAR, optional => 1 },
                        month    => { type => SCALAR, optional => 1 },
                        day      => { type => SCALAR, optional => 1 },
                        locale => { type => SCALAR | OBJECT, optional => 1 },
                      } );

    if (exists $p{locale} && ! ref $p{locale}) {
        $p{locale} = DateTime::Locale->load($p{locale})
    }

    $self->{$_} = $p{$_} for keys %p;
    return $self;
}

sub truncate {
    my $self = shift;
    my %p = validate( @_,
                      { to =>
                        { regex => qr/^(?:year|month|day)$/ },
                      },
                    );
    foreach my $f ( qw( day month year ) ) {
        last if $p{to} eq $f;
        $self->{$f} = 1;
    }
    return $self;
}

sub locale { $_[0]->{locale} }

sub is_leap_year {
    my $self = shift;
    my $year = $self->{year};
    if ($year % 4 != 3 or ($year % 100 == 27 and $year % 400 != 127)) {
        return 0;
    } else {
        return 1;
    }
}

sub year    { $_[0]->{year} }

sub month   { $_[0]->{month} }
*mon = \&month;          

sub month_0 { $_[0]->{month}-1 }
*mon_0 = \&month_0;

sub day_of_month { $_[0]->{day} }
*day  = \&day_of_month;
*mday = \&day_of_month;

sub day_of_month_0 { $_[0]->{day} - 1 }
*day_0  = \&day_of_month_0;
*mday_0 = \&day_of_month_0;

sub month_name {
    return (qw/Absolu Haha As Sable D�cervelage Gueules P�dale Clinamen
               Palotin Merdre Gidouille Tatane Phalle/)[$_[0]->{month}-1];
}

sub day_of_week {
    my $self = shift;

    if ($self->{day} == 29) {
        return undef;
    } else {
        return 1 + ($self->{day}-1) % 7;
    }
}

sub day_of_week_0 {
    my $self = shift;

    if ($self->{day} == 29) {
        return undef;
    } else {
        return +($self->{day}-1) % 7;
    }
}

sub day_name {
    my $self = shift;

    if ($self->{day} == 29) {
        my $name = 'hunyadi';
        my $n = $self->{locale}->day_names->[0];
        $name = ucfirst $name if $n eq ucfirst $n;
        return $name;
    } else {
        return $self->{locale}->day_names->[($self->day_of_week_0 || 7)-1];
    }
}

sub week_number {
    my $self = shift;

    if ($self->{day} == 29) {
        return undef;
    } else {
        return 4*($self->{month} - 1) + int(($self->{day} - 1)/7) + 1;
    }
}

sub week_year { $_[0]->year }
    
sub week { $_[0]->week_year, $_[0]->week_number }

sub day_of_year {
    my $self = shift;

    return $self->{day} + ($self->{month}-1) * 29;
}

sub day_of_year_0 {
    my $self = shift;

    return $self->{day} + ($self->{month}-1) * 29 - 1;
}

sub ymd {
    my ($self, $sep) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.3d%s%0.2d%s%0.2d",
                    $self->{year}, $sep,
                    $self->{month}, $sep,
                    $self->{day} );
}
*date = \&ymd;

sub mdy {
    my ($self, $sep) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.3d",
                    $self->{month}, $sep,
                    $self->{day}, $sep,
                    $self->{year} );
}
sub dmy {
    my ($self, $sep) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.3d",
                    $self->{day}, $sep,
                    $self->{month}, $sep,
                    $self->{year} );
}

sub datetime {
    my $self = shift;

    # EP = Ere Pataphysique
    return $self->ymd() . 'EP';
}

my %formats =
    ( 'A' => sub { $_[0]->day_name },
      'B' => sub { $_[0]->month_name },
      'C' => sub { int( $_[0]->year / 100 ) },
      'd' => sub { sprintf( '%02d', $_[0]->day_of_month ) },
      'D' => sub { $_[0]->strftime( '%m/%d/%y' ) },
      'e' => sub { sprintf( '%2d', $_[0]->day_of_month ) },
      'F' => sub { $_[0]->ymd('-') },
      'j' => sub { $_[0]->day_of_year },
      'm' => sub { sprintf( '%02d', $_[0]->month ) },
      'n' => sub { "\n" },
      't' => sub { "\t" },
      'u' => sub { $_[0]->day_of_week || 'H' },
      'U' => sub { my $w = $_[0]->week_number;
                   defined $w ? sprintf('%02d', $w) : '  ' },
      'w' => sub { my $dow = $_[0]->day_of_week;
                   defined $dow ? $dow-1 : 'H' },
      'y' => sub { sprintf( '%02d', substr( $_[0]->year, -2 ) ) },
      'Y' => sub { return $_[0]->year },
      '%' => sub { '%' },
      '*' => sub { $_[0]->feast },
    );
$formats{W} = $formats{V} = $formats{U};

sub strftime {
    my ($self, @r) = @_;

    foreach (@r) {
        s/%([%*A-Za-z])/ $formats{$1} ? $formats{$1}->($self) : $1 /ge;
        return $_ unless wantarray;
    }
    return @r;
}

sub last_day_of_month {
    my $class = shift;
    my %p = validate( @_,
                      { year   => { type => SCALAR },
                        month  => { type => SCALAR },
                        locale  => { type => SCALAR | OBJECT, optional => 1 },
                      }
                    );
    $p{day} = 29;
    return $class->new(%p);
}

sub is_imaginary {
    my $self = shift;

    return $self->{day} == 29 && $self->{month} != 11 &&
           ($self->{month} != 6 or !$self->is_leap_year);
}

sub utc_rd_values {
    my $self = shift;

    return if $self->is_imaginary;

    my ($year, $month, $day) = @{$self}{qw/year month day/};

    my $cyear = $year;
    $cyear++ if $month > 6;
    $day++ if $month > 11;

    my $rd = 683984 +                   # 7 September 1873 = 28 Phalle 0
             ($year-1) * 365 +          # normal years: 365 real days
             _floor( .25 * $cyear) -    # leap years
             _floor(($cyear+72)/100) +  # century years
             _floor(($cyear+272)/400 ) +
             + ($month - 1) * 28 + $day;
    return ($rd, $self->{rd_secs}, $self->{rd_nano});
}

sub utc_rd_as_seconds {
    my $self = shift;
    my ($rd_days, $rd_secs, $rd_nano) = $self->utc_rd_values;

    if (defined $rd_days) {
        return $rd_days*24*60*60 + $rd_secs + $rd_nano * 1e-9;
    } else {
        return undef;
    }
}

sub from_object {
    my $class = shift;
    my %p = validate( @_,
                      { object => { type => OBJECT,
                                    can => 'utc_rd_values',
                                  },
                        locale => { type => SCALAR | OBJECT,
                                      default => $class->DefaultLocale },
                      },
                       );

    $p{object} = $p{object}->clone->set_time_zone( 'floating' )
                                if $p{object}->can( 'set_time_zone' );

    my ( $rd_days, $rd_secs, $rd_nano ) = $p{object}->utc_rd_values;

    my ($y, $m, $d) = $class->_rd2ymd( $rd_days );

    return $class->new( year => $y, month => $m, day => $d,
                        rd_secs => $rd_secs||0, rd_nano => $rd_nano||0,
                        locale => $p{locale} );
}

sub _rd2ymd {
    my ($class, $rd) = @_;

    # Algorithm similar to the one on
    # http://home.capecod.net/~pbaum/date/injdalg2.htm
    # for the gregorian calendar

    # Number of days since 1 Pedale 127 (day after first extra leap day) =
    # 24-02-2000
    $rd -= 730173;

    my $a = _floor(($rd-0.25)/(100*365.2425));
    my $b = $rd - 0.25 + $a - _floor($a/4);
    my $y = _floor($b/365.25);
    my $d = $rd + $a - _floor($a/4) - _floor(365.25*$y);

    my $m;
    if ($d < 5*28 + 1) {        # Before 29 Gidouille
        $m = _floor(($d-1)/28);
        $d -= $m * 28;
    } elsif ($d == 5*28 + 1) {  # 29 Gidouille
        $m = 4;
        $d = 29;
    } elsif ($d < 366) {        # Before 29 Gueules
        $m = _floor(($d-2)/28);
        $d -= $m*28 + 1;
    } else {                    # 29 Gueules (leap day)
        $m = 12;
        $d = 29;
    }

    $y += 127;
    $m += 7;
    if ($m > 13) {
        $m -= 13;
        $y ++;
    }

    return $y, $m, $d;
}

sub from_epoch {
    my $class = shift;
    my %p = validate( @_,
                      { epoch => { type => SCALAR },
                        locale => { type => SCALAR | OBJECT,
                                      default => $class->DefaultLocale },
                      }
                    );

    my $rd = int($p{epoch}/(24*60*60) + 719163);
    
    my ($y, $m, $d) = $class->_rd2ymd( $rd );

    return $class->new( year => $y, month => $m, day => $d,
                        locale => $p{locale} );
}

sub now { shift->from_epoch( epoch => (scalar time), @_ ) }

sub _add_overload {
    my ($dt, $dur, $reversed) = @_;
    ($dur, $dt) = ($dt, $dur) if $reversed;

    my $new = $dt->clone;
    $new->add_duration($dur);
    return $new;
}

sub _subtract_overload
{
    my ( $date1, $date2, $reversed ) = @_;
    ($date1, $date2) = ($date2, $date1) if $reversed;

    if ( UNIVERSAL::isa($date2, 'DateTime::Duration') ) {
        my $new = $date1->clone;
        $new->add_duration( $date2->inverse );
        return $new;
    } else {
        return $date1->subtract_datetime($date2);
    }
}

sub add {return shift->add_duration(DateTime::Duration->new(@_)) }

sub subtract { return shift->subtract_duration(DateTime::Duration->new(@_)) }

sub subtract_duration { return $_[0]->add_duration( $_[1]->inverse ) }

sub add_duration {
    my ($self, $dur) = @_;

    my %deltas = $dur->deltas;

    $self->{day} += $deltas{days} if $deltas{days};
    $self->{month} += $deltas{months} if $deltas{months};

    if ($self->{day} < 1 or $self->{day} > 29) {
        $self->{month} += _floor(($self->{day}-1)/29);
        $self->{day} %= 29;
    }
    if ($self->{month} < 1 or $self->{month} > 13) {
        $self->{year} += _floor(($self->{month}-1)/13);
        $self->{month} %= 13;
    }
    return $self;
}

sub subtract_datetime {
    my ($self, $dt) = @_;

    my $days_diff = ($self->year  - $dt->year ) * 377 +
                    ($self->month - $dt->month) * 29 +
                    ($self->day   - $dt->day  );
    return DateTime::Duration->new( days => $days_diff );
}

use constant INFINITY     =>       100 ** 100 ** 100 ;
use constant NEG_INFINITY => -1 * (100 ** 100 ** 100);
            
sub _compare_overload
{       
    # note: $_[1]->compare( $_[0] ) is an error when $_[1] is not a
    # DateTime (such as the INFINITY value)
    return $_[2] ? - $_[0]->compare( $_[1] ) : $_[0]->compare( $_[1] );
}

sub compare
{
    my ($class, $dt1, $dt2) = ref $_[0] ? (undef, @_) : @_;

    return undef unless defined $dt2;

    return -1 if ! ref $dt2 && $dt2 == INFINITY;
    return  1 if ! ref $dt2 && $dt2 == NEG_INFINITY;

    $dt2 = $class->from_object( object => $dt2 )
        unless $dt2->isa('DateTime::Calendar::Pataphysical');

    return $dt1->year <=> $dt2->year || $dt1->month <=> $dt2->month ||
           $dt1->day  <=> $dt2->day;
}


my @feasts;

sub feast {
    return $feasts[ $_[0]->day_of_year_0 ];
}

# Feasts from
# http://perso.wanadoo.fr/mexiqueculture/nouvelles6-latumba.htm
@feasts = split /\n+/, <<EOF;
Nativit� d'Alfred Jarry
St Ptyx, silentiaire (Abolition de)
St Ph�nix, solipsiste et St Hyx, factotum
St Lucien de Samosate, voyageur
St Bardamu, voyageur
Ste V�rola, assistante sociale
St Alambic, abstracteur
Absinthe, ci-devant St Alfred
Descente du St Esprit (de Vin)
Dilution
Ste Pur�e, sportswoman
Vide
St Canterel, l'illuminateur
St Sophrotatos l'Arm�nien, pataphysicien
�thernit�
St Ibicrate le G�om�tre, pataphysicien
C�phalorgie
Fl�tes de Pan
Stes Grues, ophiophiles
Ste M�lusine, souillarde de cuisine
St Venceslas, duc
Emmanuel Dieu
Ste Varia-Miriam, amphibie
Sts Rakirs et Rastrons, porte-c�telettes
Nativit� de Sa Magnificence Opach
St Joseb, notaire � la mode de Bretagne
Stes Gigolette et Gaufrette, dogaresses
Xylostomie
Le Jet Musical

L'�ge du Dr Faustroll
Dissolution d'E. Poe, dinomythurge
St Gibus, franc-ma�on
Ste Berthe de Courri�re, �g�rie
Ste Belgique, nourrice
Ste Tourte, lyrique et Ste B�vue, sociologue
St Prout, abb�
F�te du Haha
Tautologie
St Panmuphle, huissier
Sortie de St L. Cranach, apocalypticien
St Cosinus, savant
Bse Fenouillard, sainte famille
Exhibition de la Daromphe
Nativit� de l'OEstre, artificier
Ste Vadrouille, embl�me
St Homais d'Aquin, prudhomme
Nativit� de Sa Magnificence le baron Mollet (St Pipe)
St Rapha�l, ap�ritif et philistin
Strangulation de Bosse-de-Nage
Zimzoum de Bosse-de-Nage
R�surrection de Bosse-de-Nage
Chapeau de Bosse-de-Nage
St Cl. Terrasse, musicien des Phynances
St J.-P. Brisset, philologue, prince des penseurs
Comm�moration du Cure-dent
Occultation d'Alfred Jarry
Fuite d'Ablou
Mar�e Terrestre

Nativit� de Pantagruel
Ste Rrose S�lavy, h�ro�ne
Couronnement de Lord Patchogue, miroitier
St Cravan, boxeur
St Van Meegeren, faussaire
St Omnibus, satyre
St Cyrano de Bergerac, explorateur
St Rimbe, oisif
�quarrissage pour tous
St Abstrait, bourreau
St Ossian, barde postiche
Dispute du Signe + et du Signe -
Moustaches du Dr Faustroll
St P. Bonnard, peintre des Phynances
Navigation du Dr Faustroll
St Cap, captain
St Pangloss, humoriste passif
St Chambernac, pauvriseur
St Courtial des P�reires, a�rostier et inventeur
St Olibrius, augure
St Possible, schizophr�ne
St Lautr�amont
St Quincey, critique d'art
St Berbiguier, martyr
St Lewis Carroll, professeur
St Mensonger, �v�que
Ste Visit�, fille du pr�c�dent
Nativit� de St Swift, chanoine
Travers�e du Miroir

Noces de Balkis et de Salomon
St Doublemain, id�ologue
St Phlegmon, doctrinaire
Ste Barbe (femme �), femme-canon
Ste Savate, avocate
St Navet et Ste Perruque, humanistes
St Birbe, juge
Conception du P. Ubu (A. J.)
St Sagouin, homme d'�tat
Exaltation d'Ubu Roi (Ubu d'hiver)
Nativit� de St Grabbe, scherziste
Ste Choupe, m�re de famille
St Flaive, concierge
Don Quichotte, champion du monde
Khurmookum du Dr Faustroll
St Nul, exempt
St Moyen, fran�ais
Ste Lurette, joconde
Gravidit� de M�re Ubu
St Sabre, allopathe
Ste Tape, pompette
C�sar - Antechrist
Ste Viole, vierge et martyre
Ste Pochet�e, gouvernante
Nativit� de l'Arch�opt�ryx
Monsieur Sisyphe
St Tic, conjoint
St Cervelas, penseur
Aleph

St Alaodine, virtuose
Sts Hassassins, praticiens
Astu
D�cervelage
Sts Giron, Pile et Cotice, palotins
Sts Polonais, prol�taires
Sts For�ats, poliorc�tes
St Bordure, capitaine
Dormition de Jacques Vach�, interpr�te
Drapaud (�rection du)
St Eustache, lib�rateur
St Landru, gyn�cologue
St Guillotin, m�decin
Sts 4 Sans-Cou, enchanteurs
Conscience d'Ubu
St Mauvais, sujet
St Mandrin, po�te et philosophe
Sts Pirates et Flibustiers, thaumaturges
St et Ste Cartouche, v�t�rinaires
St Outlaw, aristocrate
Chaire du Dr FaustrolL
Ostention du B�ton � Physique
St Tank, animal
St Weidman, patriarche
St Petiot, expert
Escrime
Sts Chemins de fer, assassins
Repopulation
Lit de Procruste

D�pucelage de M�re Ubu
St Sigisb�e, eunuque
St Anthropo�de, policier
Ste Goule ou Gudule, institutrice
Ste Gale, abbesse
Ste Touche, postulante
St Gueule, abb�
F�te de la Chandelle Verte
Ste Cr�pe, la�que
St Pr�servatif, bedeau
St Baobab, c�libataire
St Membre, compilateur
Copulation
Nativit� de St J. Verne, globe-trotter en chambre
Alice au Pays des Merveilles
St M�nchhausen, baron
Le B�trou, th�urge
Nativit� de St Deibler, prestidigitateur
St Sade �s liens
St Lafleur, valet
Lavement
St Sexe, stylite
Occultation de St J. Torma, euphoriste
Conversion de St Matorel, bateleur
Ste Marmelade, inspir�e
L'Amour Absolu, deliquium
Ste Tabagie, cosmog�ne
Sts Hylactor et Pamphagus
Mouvement Perp�tuel

�rection du Surm�le
St Andr� Marcueil, asc�te cycliste
St Ellen, hile
St Michet, id�aliste
St Ouducul, trouv�re
Vers Belges
St Gavroche, forain
La Machine � Inspirer l'Amour
St Remezy, �v�que in partibus
Nativit� de St Tancr�de, jeune homme
Testament de P. Uccello, le mal illumin�
St Hari Seldon, psychohistorien galactique
Ste Valburge, succube
Sabbat
Sts Adelphes, �sot�ristes
Sts Templiers, adeptes
St Dricarpe, pros�lyte
St Nosocome, carabin
Ste Goutte, f�te militaire
Ste Cuisse, dame patronnesse
St Inscrit, Converti
St Sengle, d�serteur
St Masquarade, uniforme
Nativit� de St St�phane, faune
St Poligraf Poligrafovitch, chien
St P�le, mineur
St Valens, fr�re onirique
D�dicace du Tripode
Bse Escampette, dynamiteuse

St Ablou, page et St Haldern, duc
Sts Hiboux, ma�tres-chanteurs
La Mandragore, solan�e andro�de
St Pagne, confident
Sts Aster et Vulpian, violateurs du N�ant
St Ganym�de, professionnel
La Main de Gloire
La Machine � Peindre
Ste Trique, lunatique
R�mission des Poissons
St Maquereau, intercesseur
St Georges Dazet, poulpe au regard de soie
Nativit� de Maldoror, corsaire aux cheveux d'or
Sortie d'A. D�rer, herm�tiste
Invention de la 'Pataphysique
Exit St Domenico Theotocopouli, el Greco
St Hi�ronymus Bosch, d�monarque
Les 27 �tres Issus des Livres Pairs
St Barbeau, procureur et Ste Morue, juste
Capture du Fourneau
St Docteur Moreau, insulaire
F�te des Poly�dres
Locus Solus
St Tupetu de Tupetu, organisateur de loteries
Exit St Goya, alchimiste
St Escargot, sybarite
Ste Hure de Chastet�, p�nitente
St Turgescent, iconoclaste
Cymbalum Mundi

Sts Crocodiles, crocodiles
F�te des �cluses
Sts Trolls, pantins
Ste Susan Calvin, docteur
Ste Poign�e, veuve et Ste Jutte, recluse
Ste Oneille, gourgandine
St F�n�on �s Liens
St Bougrelas, prince
Sts Boleslas et Ladislas, polonais
St Forficule, Barnabite
Explosion du Palotin
R�probation du Travail
Esquive de St L�onard (de Vinci), illusionniste
St �quivoque, sans-culotte
Adoration du Pal
D�ploration de St Achras, �leveur de Poly�dres
St Macrotatoure, caudataire
Canotage
Occultation de St Gauguin, oc�anide
St Ti Belot, s�ide
Occultation de Sa Magnificence le Dr Sandomir
Sts Palotins des Phynances
Sts Quatrezoneilles, Herdanpo, Mousched-Gogh, palotins
Ste Lumelle, �cuy�re
Sts Potassons, acolythes
Ste Pr�tentaine, rosi�re
St Foin, coryph�e
Nativit� de St Satie, Grand Parcier de l'�glise d'Art
Erratum

Accouchement de Ste Jeanne, papesse
Le Moutardier du Pape
St Si�ge, sous-pape
Nativit� de St H. Rousseau, douanier
St Crouducul, troupier
St Cucufat, m�c�ne
Nativit� de M. Plume, propri�taire
Cocuage de M. le P. Ubu
Vidange
St Barbapoux, amant
St Memnon, vidangeur
Stes Miches, cat�chum�nes
Ste Lunette, solitaire
St Sphincter, prof�s
Sts Serpents d'Airain
Nativit� de St Donatien A. Fran�ois
St Woland, professeur
St Anal, cordelier et Ste Foire, anagogue
Ste F�tatoire, super
Ste Colombine, expurg�e
Ste Pyrotechnie, illumin�e
Ontog�nie Pataphysique
Interpr�tation de L'Umour
Ste Purge, sage-femme
Apparition D'Ubu Roi
Ste Barbaque, na�ade
Sts Courts et Longs, gendarmes
St Raca, cagot
D�faite du Mufle

Ste Bouzine, esprit
St Lucullus, amateur (Bloomsday)
Ste Dondon, amazone
Ste Tripe, r�publicaine
St Ugolin, mansuet
St Dieu, retrait�
St B�b� Toutout, �vang�liste
Ste Boudouille, bayad�re
Ste Outre, psychiatre
St Boudin, recteur
Sacre de Talou VII, empereur du Ponuk�l�
Ste Confiture, d�vote et Ste Cliche, donatrice
Sts Instintestins, conseillers intimes
St Colon, artilleur
Ste Giborgne, v�n�rable
St Inventaire, po�te
Ste Femelle, technicienne
Visitation de M�re Ubu
St Sein, tautologue
St P�rin�e, z�lateur
St Sp�culum, confesseur
F�te de Gidouille
St Ombilic, gymnosophiste
St Gris-gris, ventre
St Bouffre, pontife
Ste Goulache, odalisque
Ste Gandouse, hygi�niste
Poche du P�re Ubu
Nom d'Ubu

F�te du P. Ubu (Ubu d'�t�)
Comm�moration du P. �b�
Ste Crapule, puriste et St Fantomas, archange
Ascension du Mouchard, statisticien, psychiatre et policier
St Arsouille, patricien
Sts Robot et Cornard, citoyens
St Biribi, taulier
Susception du Croc � Merdre
Sts �crase-Merdre, sectateurs
Sts Pieds Nickel�s, trinit�
Stes Canicule et Canule, jouvencelles
Sts Cannibales, philanthropes
St Dada, proph�te
Ste Anne, p�lerine, �nergum�ne
Procession aux Phynances
Transfiguration de St V. van Gogh, transmutateur
Ste Flamberge, voyante
St Trou, chauffeur
Ste Taloche, matrone
St Tiberge, fr�re qu�teur
Sts Catoblepas, lord et Anoblepas, amiral
Ubu �s Liens
St Pissembock, oncle
St Pissedoux, caporal des hommes libres
St Panurge, moraliste
St Gl�, neurologue-ali�niste
St Pistolet � Merdre, jubilaire
Nativit� de St Bruggle
Le soleil solide froid

St Chibre, planton
Ste Ruth, z�latrice
St Zebb, passe-partout
St Mnester, confesseur
Assomption de Ste Messaline
Penis Angelicus
St Patrobas, pompier
Ste L�da, ajusteuse
St Godemich�, �conome
Ste Nitouche, orante
Ste L�chefrite, botteuse
Ste Andouille, amphibologue
Ste Bitre, ouvreuse et St �talon, couvreur
Bataille de Morsang
Mort de Dionysos, surhomme
Nativit� de St Vibescu, poh�te et Comm�moration de Ste Cuculine d'Anc�ne
Ste Gallinac�e, cocotte
St Lingam, bouche-trou
St Pr�lote, capucin
St Pie VIII, navigant
St Erbrand, polytechnicien
Ste Dragonne, pyrophage
St Lazare, gare
Ste Orchid�e, aumoni�re
Nativit� apparente d'Artaud le Momo
Disparition de l'Ancien Breughel, incendiaire
St Priape, franc-tireur
Transfixion de Ste Messaline
Le Term�s
EOF

1;
__END__

sub add_duration {
    my ($self, $dur) = @_;

    my $dm = $dur->delta_months;
    my $dd = $dur->delta_days;

    $self->{day}   += $dd;
    $self->{month} += _floor(($self->{day}-1)/29);
    $self->{day}    = ($self->{day}-1)%29 + 1;

    $self->{month} += $dm;
    $self->{year}  += _floor(($self->{year}-1)/13);
    $self->{month}  = ($self->{month}-1)%13 + 1;

    return $self;
}


1;
__END__

=head1 NAME

DateTime::Calendar::Pataphysical - Dates in the pataphysical calendar

=head1 SYNOPSIS

  use DateTime::Calendar::Pataphysical;

  $dt = DateTime::Calendar::Pataphysical->new( year  => 1752,
                                               month => 10,
                                               day   => 4 );

=head1 DESCRIPTION

DateTime::Calendar::Pataphysical is the implementation of the
pataphysical calendar. Each year in this calendar contains 13 months of
29 days. This regularity makes this a convenient alternative for the
irregular Gregorian calendar.

This module is designed to be easy to use in combination with
DateTime. Most of its methods correspond to a DateTime method of the
same name. 

=head1 METHODS

=over 4

=item * new( ... )

This class method accepts parameters for each date and time component:
"year", "month", "day".  Additionally, it accepts a "locale"
parameter.

The "rd_secs" parameter is also accepted. This parameter is only useful
in conversions to other calendars; this calendar does not use its value.

=item * from_epoch( epoch => $epoch, ... )

This class method can be used to construct a new object from
an epoch time instead of components.  Just as with the C<new()>
method, it accepts a "locale" parameter.

=item * now( ... )

This class method is equivalent to calling C<from_epoch()> with the
value returned from Perl's C<time()> function.

=item * from_object( object => $object, ... )

This class method can be used to construct a new object from
any object that implements the C<utc_rd_values()> method.  All
C<DateTime::Calendar> modules must implement this method in order to
provide cross-calendar compatibility.  This method accepts a
"locale" parameter.

The time part of $object is stored, and will only be used if the created
object is converted to another calendar. Only the date part of $object
is used to calculate the Pataphysical date. This calculation is based on
the local time and date of $object.

=item * last_day_of_month( ... )

This constructor takes the same arguments as can be given to the
C<now()> method, except for "day".  Additionally, both "year" and
"month" are required.

=item * clone

This object method returns a replica of the given object.

=item * year

Returns the year.

=item * month

Returns the month of the year, from 1..13.

=item * month_name

Returns the name of the current month.

=item * day_of_month, day, mday

Returns the day of the month, from 1..29.

=item * day_of_week, wday, dow

Returns the day of the week as a number, from 1..7, with 1 being
Sunday and 7 being Saturday.

=item * day_name

Returns the name of the current day of the week.

=item * day_of_year, doy

Returns the day of the year.

=item * ymd( $optional_separator ), date

=item * mdy( $optional_separator )

=item * dmy( $optional_separator )

Each method returns the year, month, and day, in the order indicated
by the method name.  Years are zero-padded to four digits.  Months and
days are 0-padded to two digits.

By default, the values are separated by a dash (-), but this can be
overridden by passing a value to the method.

=item * datetime

Equivalent to

  $dt->ymd('-') . 'EP'

=item * is_leap_year

This method returns a true or false indicating whether or not the
datetime object is in a leap year.

=item * week

 ($week_year, $week_number) = $dt->week

Returns information about the calendar week which contains this
datetime object. The values returned by this method are also available
separately through the week_year and week_number methods.

=item * week_year

Returns the year of the week. In the pataphysical calendar, this is
equal to the year of the date, as all weeks fall in one year only.

=item * week_number

Returns the week of the year, from 1..53.

The 29th of each month falls outside of any week; week_number returns
undef for these dates.

=item * utc_rd_values

Returns the current UTC Rata Die days and seconds as a two element
list.  This exists primarily to allow other calendar modules to create
objects based on the values provided by this object.

=item * utc_rd_as_seconds

Returns the current UTC Rata Die days and seconds purely as seconds.
This is useful when you need a single number to represent a date.

=item * strftime( $format, ... )

This method implements functionality similar to the C<strftime()>
method in C.  However, if given multiple format strings, then it will
return multiple elements, one for each format string.

See L<DateTime> for a list of all possible format specifiers. This
module implements all specifiers related to dates. There is one
additional specifier: C<%*> represents the feast of that date.

=item * feast

Returns the feast or vacuation of the given date.

=item * is_imaginary

Returns true or false indicating whether the datetime object represents an
imaginary date.

=item * set( .. )

This method can be used to change the local components of a date time,
or its locale.  This method accepts any parameter allowed by the
C<new()> method.

=item * truncate( to => ... )

This method allows you to reset some of the local time components in
the object to their "zero" values.  The "to" parameter is used to
specify which values to truncate, and it may be one of "year",
"month", or "day".

=item * add_duration( $duration_object )

This method adds a C<DateTime::Duration> to the current datetime.  See
the L<DateTime::Duration|DateTime::Duration> docs for more detais.

=item * add( DateTime::Duration->new parameters )

This method is syntactic sugar around the C<add_duration()> method.  It
simply creates a new C<DateTime::Duration> object using the parameters
given, and then calls the C<add_duration()> method.

=item * subtract_duration( $duration_object )

When given a C<DateTime::Duration> object, this method simply calls
C<invert()> on that object and passes that new duration to the
C<add_duration> method.

=item * subtract( DateTime::Duration->new parameters )

Like C<add()>, this is syntactic sugar for the C<subtract_duration()>
method.

=item * subtract_datetime( $datetime )

This method returns a new C<DateTime::Duration> object representing
the difference between the two dates.

=item * compare

  $cmp = DateTime->compare($dt1, $dt2);

  @dates = sort { DateTime->compare($a, $b) } @dates;

Compare two DateTime objects.  The semantics are compatible with
Perl's C<sort()> function; it returns -1 if $a < $b, 0 if $a == $b, 1
if $a > $b.

Of course, since DateTime objects overload comparison operators, you
can just do this anyway:

  @dates = sort @dates;

=back

=head1 BUGS

=over 4

=item * Adding a week to a date is exactly equivalent to adding seven days
in this module because of the way DateTime::Duration is implemented.
The Hunyadis are not taken into account.

=item * from_epoch() and now() probably only work on Unix.

=back

=head1 SUPPORT

Support for this module is provided via the datetime@perl.org email
list. See http://lists.perl.org/ for more details.

=head1 AUTHOR

Eugene van der Pijll <pijll@gmx.net>

=head1 COPYRIGHT

Copyright (c) 2003 Eugene van der Pijll.  All rights reserved.  This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 SEE ALSO

L<DateTime>

datetime@perl.org mailing list

=cut
