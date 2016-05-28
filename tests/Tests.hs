{-# LANGUAGE FlexibleInstances #-}
import Test.Tasty.QuickCheck
import Test.Tasty

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Control.Applicative
import Control.Monad

import Data.Git.Storage.Object
import Data.Git.Storage.Loose
import Data.Git.Ref
import Data.Git.Revision
import Data.Git.Types
import Data.Hourglass

import Data.Maybe

-- for arbitrary instance to generate only data that are writable
-- to disk. i.e. no deltas.
data ObjNoDelta hash = ObjNoDelta (Object hash)
    deriving (Eq)

instance Show (ObjNoDelta hash) where
    show (ObjNoDelta o) = show o

arbitraryBS size = B.pack . map fromIntegral <$> replicateM size (choose (0,255) :: Gen Int)
arbitraryBSno0 size = B.pack . map fromIntegral <$> replicateM size (choose (1,255) :: Gen Int)
arbitraryBSasciiNoSpace size = B.pack . map fromIntegral <$> replicateM size (choose (0x21,0x7f) :: Gen Int)
arbitraryBSascii size = B.pack . map fromIntegral <$> replicateM size (choose (0x20,0x7f) :: Gen Int)
arbitraryBSnoangle size = B.pack . map fromIntegral <$> replicateM size (choose (0x40,0x7f) :: Gen Int)

arbitraryEntname size = entName . B.pack . map fromIntegral <$> replicateM size range
  where range :: Gen Int
        range = oneof [ choose (0x21, 0x2e) -- remove 0x2f (slash)
                      , choose (0x30, 0x7f)
                      ]

instance Arbitrary (Ref SHA1) where
    arbitrary = fromBinary <$> arbitraryBS 20

arbitraryMsg = arbitraryBSno0 1
arbitraryLazy = L.fromChunks . (:[]) <$> arbitraryBS 40

arbitraryRefList :: Gen [Ref SHA1]
arbitraryRefList = replicateM 2 arbitrary

arbitraryEnt :: Gen (TreeEnt SHA1)
arbitraryEnt = liftM3 (,,) arbitrary (arbitraryEntname 23) arbitrary
arbitraryEnts = choose (1,2) >>= \i -> replicateM i arbitraryEnt

instance Arbitrary TimezoneOffset where
    arbitrary = TimezoneOffset <$> choose (-11*60, 12*60)
instance Arbitrary Elapsed where
    arbitrary = Elapsed . Seconds <$> choose (0,2^32-1)
instance Arbitrary GitTime where
    arbitrary = GitTime <$> arbitrary <*> arbitrary
instance Arbitrary ModePerm where
    arbitrary = ModePerm <$> elements [ 0o644, 0o664, 0o755, 0 ]
instance Arbitrary RevModifier where
    arbitrary = oneof
        [ RevModParent . getPositive <$> arbitrary
        , RevModParentFirstN . getPositive <$> arbitrary
        , RevModAtType <$> arbitraryType
        , RevModAtDate <$> arbitraryDate
        --, RevModAtN . getPositive <$> arbitrary
        ]

arbitraryDate = elements ["yesterday","29-Jan-1982","5 days ago"]
arbitraryType = elements ["commit","tree"]

instance Arbitrary Revision where
    arbitrary = do
        s   <- choose (1,40) >>= flip replicateM (elements ['a'..'z'])
        rms <- choose (1,4) >>= flip replicateM arbitrary
        return $ Revision s rms

arbitraryName = liftM3 Person (arbitraryBSnoangle 16)
                              (arbitraryBSnoangle 16)
                              arbitrary

arbitraryObjTypeNoDelta = oneof [return TypeTree,return TypeBlob,return TypeCommit,return TypeTag]

arbitrarySmallList = frequency [ (2, return []), (1, resize 3 arbitrary) ]

instance Arbitrary (Commit SHA1) where
    arbitrary = Commit <$> arbitrary <*> arbitraryRefList <*> arbitraryName <*> arbitraryName <*> return Nothing <*> arbitrarySmallList <*> arbitraryMsg

instance Arbitrary CommitExtra where
    arbitrary = CommitExtra <$> arbitraryBSasciiNoSpace 80 <*> arbitraryMsg

instance Arbitrary (Tree SHA1) where
    arbitrary = Tree <$> arbitraryEnts

instance Arbitrary (Blob SHA1) where
    arbitrary = Blob <$> arbitraryLazy

instance Arbitrary (Tag SHA1) where
    arbitrary = Tag <$> arbitrary <*> arbitraryObjTypeNoDelta <*> arbitraryBSascii 20 <*> arbitraryName <*> arbitraryMsg

instance Arbitrary (ObjNoDelta SHA1) where
    arbitrary = ObjNoDelta <$> oneof
        [ toObject <$> (arbitrary :: Gen (Commit SHA1))
        , toObject <$> (arbitrary :: Gen (Tree SHA1))
        , toObject <$> (arbitrary :: Gen (Blob SHA1))
        , toObject <$> (arbitrary :: Gen (Tag SHA1))
        ]

prop_object_marshalling_id :: ObjNoDelta SHA1 -> Bool
prop_object_marshalling_id (ObjNoDelta obj) =
    let unmarshall = looseUnmarshall :: L.ByteString -> Object SHA1
     in obj `assertEq` (unmarshall $ looseMarshall obj)
    where assertEq a b
            | show a == show b    = True
            | otherwise = error ("not equal:\n"  ++ show a ++ "\ngot: " ++ show b)

refTests =
    [ testProperty "hexadecimal" (marshEqual (fromHex . toHex :: Ref SHA1 -> Ref SHA1))
    , testProperty "binary" (marshEqual (fromBinary . toBinary :: Ref SHA1 -> Ref SHA1))
    , testProperty "ref" $ marshEqual (fromString . show :: Revision -> Revision)
    ]
    where
        marshEqual t ref = ref `assertEq` t ref
        assertEq a b
            | a == b    = True
            | otherwise = error ("expecting: " ++ show a ++ " got: " ++ show b)

objTests =
    [ testProperty "unmarshall.marshall==id" prop_object_marshalling_id
    ]

main = defaultMain $ testGroup "hit"
    [ testGroup "ref marshalling" refTests
    , testGroup "object marshalling" objTests
    ]
