import Test.Tasty.QuickCheck
import Test.Tasty

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Control.Applicative
import Control.Monad

import Data.Git.Storage.Object
import Data.Git.Storage.Loose
import Data.Git.Ref
import Data.Git.Types
import Data.Hourglass

import Data.Maybe

-- for arbitrary instance to generate only data that are writable
-- to disk. i.e. no deltas.
data ObjNoDelta = ObjNoDelta Object
    deriving (Eq)

instance Show ObjNoDelta where
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

instance Arbitrary Ref where
    arbitrary = fromBinary <$> arbitraryBS 20

arbitraryMsg = arbitraryBSno0 1
arbitraryLazy = L.fromChunks . (:[]) <$> arbitraryBS 40

arbitraryRefList :: Gen [Ref]
arbitraryRefList = replicateM 2 arbitrary

arbitraryEnt :: Gen TreeEnt
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

arbitraryName = liftM3 Person (arbitraryBSnoangle 16)
                              (arbitraryBSnoangle 16)
                              arbitrary

arbitraryObjTypeNoDelta = oneof [return TypeTree,return TypeBlob,return TypeCommit,return TypeTag]

arbitrarySmallList = frequency [ (2, return []), (1, resize 3 arbitrary) ]

instance Arbitrary Commit where
    arbitrary = Commit <$> arbitrary <*> arbitraryRefList <*> arbitraryName <*> arbitraryName <*> return Nothing <*> arbitrarySmallList <*> arbitraryMsg

instance Arbitrary CommitExtra where
    arbitrary = CommitExtra <$> arbitraryBSasciiNoSpace 80 <*> arbitraryMsg

instance Arbitrary Tree where
    arbitrary = Tree <$> arbitraryEnts

instance Arbitrary Blob where
    arbitrary = Blob <$> arbitraryLazy

instance Arbitrary Tag where
    arbitrary = Tag <$> arbitrary <*> arbitraryObjTypeNoDelta <*> arbitraryBSascii 20 <*> arbitraryName <*> arbitraryMsg

instance Arbitrary ObjNoDelta where
    arbitrary = ObjNoDelta <$> oneof
        [ toObject <$> (arbitrary :: Gen Commit)
        , toObject <$> (arbitrary :: Gen Tree)
        , toObject <$> (arbitrary :: Gen Blob)
        , toObject <$> (arbitrary :: Gen Tag)
        ]

prop_object_marshalling_id (ObjNoDelta obj) = obj `assertEq` (looseUnmarshall $ looseMarshall obj)
    where assertEq a b
            | show a == show b    = True
            | otherwise = error ("not equal:\n"  ++ show a ++ "\ngot: " ++ show b)

refTests =
    [ testProperty "hexadecimal" (marshEqual (fromHex . toHex))
    , testProperty "binary" (marshEqual (fromBinary . toBinary))
    ]
    where
        marshEqual t ref = ref == t ref

objTests =
    [ testProperty "unmarshall.marshall==id" prop_object_marshalling_id
    ]

main = defaultMain $ testGroup "hit"
    [ testGroup "ref marshalling" refTests
    , testGroup "object marshalling" objTests
    ]
